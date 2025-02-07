use std::cmp::{PartialOrd, Ordering};
use std::usize;
use crate::parser::parser::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Type {
    Int(u8)
}
impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int(x), Type::Int(y)) => *x == *y,
            _ => false
        }
    }
}
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
        match (self, other) {
            (Type::Int(x), Type::Int(y)) => {
                if x > y { Some(Ordering::Greater) }
                else if x < y { Some(Ordering::Less) }
                else { Some(Ordering::Equal) }
            },
            _ => None
        }
    }
}

pub struct TypeContext {
    fnmap: HashMap<String, (Vec<Type>, Type)>,
}
impl TypeContext {
    pub fn new(prog: &Program) -> TypeContext {
        // Create map for function types
        let mut fnmap = HashMap::new();
        // Add function types to map
        for (key, value) in prog {
            fnmap.insert(
                key.clone(), 
                (value.0.iter().map(|r| r.1.clone()).collect(), value.1.clone())
            );
        }
        // Return context
        TypeContext { fnmap }
    }
    pub fn get_fn(&self, s: &String) -> Option<&(Vec<Type>, Type)> {
        self.fnmap.get(s)
    }
}

fn type_error(msg: String, loc: &(usize, usize)) -> String {
    "Type Error at ".to_string() + &loc.0.to_string() + ":" + &loc.1.to_string() + ": " + &msg
}

pub fn synth_type(c: &TypeContext, e: &Expression) -> Result<Type, String> {
    match &e.0 {
        ExpressionBody::IntLiteral(s, m) => {
            // Return OK Int by default
            Ok(Type::Int(
                // If number is positive
                if !s {
                    // Check smallest type that fits
                    if *m <= (i8::MAX as usize) { 3 }
                    else if *m <= (i16::MAX as usize) { 4 }
                    else if *m <= (i32::MAX as usize) { 5 }
                    else if *m <= (i64::MAX as usize) { 6 }
                    else { return Err(type_error("Int literal too large".to_string(), &e.1)) }
                } 
                // Number is negative
                else {
                    // Check largest negative type that fits
                    // Since 2's compliment, largest negative = largest positive + 1
                    if *m <= (i8::MAX as usize + 1) { 3 }
                    else if *m <= (i16::MAX as usize + 1) { 4 }
                    else if *m <= (i32::MAX as usize + 1) { 5 }
                    else if *m <= (i64::MAX as usize + 1) { 6 }
                    else { return Err(type_error("Int literal too large".to_string(), &e.1)) }
                }
            ))
        },
        ExpressionBody::CallExpression(f, args) => {
            // Get function input and output types
            let ftype = match c.get_fn(&f) {
                Some(x) => x,
                _ => return Err(type_error("Function does not exist: ".to_string() + &f, &e.1))
            };
            // Check that same number of args as parameters
            if args.len() != ftype.0.len() { 
                return Err(type_error("Invalid function call: ".to_string() + &f, &e.1)) 
            };
            // Check that arguments have correct type
            for (param, arg) in ftype.0.iter().zip(args) {
                if check_type(param, c, arg).is_err() {
                    return Err(type_error("Invalid argument: ".to_string() + &f, &e.1))
                }
            };
            // Return output type of function
            Ok(ftype.1.clone())
        },
        ExpressionBody::BopExpression(_, e1, e2) => {
            // Synth types for e1 and e2
            let e1_type = synth_type(c, e1.as_ref())?;
            let e2_type = synth_type(c, e2.as_ref())?;
            // Return most general type
            if e1_type > e2_type {
                Ok(e1_type)
            } else {
                Ok(e2_type)
            }
        },
        ExpressionBody::AsExpression(e1, t) => {
            // Type check e1 against t
            if !check_type(t, c, e1.as_ref())? { return Err(type_error("Invalid typecast".to_string(), &e1.as_ref().1)) }
            // Return t
            Ok(t.clone())
        },
        ExpressionBody::UopExpression(_, e1) => {
            // Synth type for e1
            let e1_type = synth_type(c, e1.as_ref());
            // Return
            e1_type
        }
        _ => panic!()
    }
}

pub fn check_type(t: &Type, c: &TypeContext, e: &Expression) -> Result<bool, String> {
    match &e.0 {
        ExpressionBody::IntLiteral(_, _) => {
            // Synthesize type for integer
            let t_synth = synth_type(c, e)?;
            // Check that synthesized type is a subtype of t
            Ok(t_synth <= *t)
        },
        _ => panic!()
    }
}