use std::{collections::HashMap, sync::atomic::{AtomicUsize, Ordering}};

use crate::parser::parser::Expression;

#[macro_export]
macro_rules! sub {
    [] => {
        Substitution::new()
    };
    [$($e1:expr => $e2:expr),+] => {
        {
        let mut sub = Substitution::new();
        $(
            sub.insert($e1, $e2);
        )+
        sub
        }
    };
}

#[macro_export]
macro_rules! var {
    ($l:literal) => {
        MonoType::Variable($l)
    }
}

#[macro_export]
macro_rules! app {
    ($e1:expr $(=> $($e2:expr),+)?) => {
        MonoType::Application(
            $e1,
            vec![ $($($e2),+)? ]
        )
    }
}

#[derive(Clone, Debug)]
pub enum MonoType {
    Variable(usize),
    Application(TypeName, Vec<MonoType>)
}
impl PartialEq for MonoType {
    fn eq(&self, x: &Self) -> bool {
        match (self, x) {
            // Both variables, check IDs
            (MonoType::Variable(x1), MonoType::Variable(x2)) => x1 == x2,
            // Both applications, check names and contents
            (MonoType::Application(n1, v1), MonoType::Application(n2, v2)) => {
                n1 == n2 && 
                v1.len() == v2.len() && 
                v1.iter().zip(v2).all(|(r1, r2)| r1 == r2)
            },
            // Not same type, false
            _ => false
        }
    }
}
impl MonoType {
    pub fn contains(&self, t2: &MonoType) -> bool {
        // Check self
        match self {
            // Self is type application
            MonoType::Application(_, v) => {
                // Any parameters contain t2
                v.iter().any(|r| r.contains(t2))
            },
            // Self is variable
            _ => self == t2
        }
    }
    pub fn unify(&self, t2: &MonoType) -> Result<Substitution, String> {
        match (self, t2) {
            // Both variables
            (MonoType::Variable(x1), MonoType::Variable(x2)) => {
                // Same variable, don't map
                if x1 == x2 {
                    Ok(sub![])
                }
                // Different variables, map self to t2 
                else {
                    Ok(sub![*x1 => t2.clone()])
                }
            },
            // Variable and application
            (MonoType::Variable(x1), _) => {
                // Check for infinite type
                if t2.contains(self) { return Err("Infinite type detected".to_string()) }
                // Map self to t2
                Ok(sub![*x1 => t2.clone()])
            }
            // Any and variable
            (_, MonoType::Variable(_)) => {
                // Unify b and a
                t2.unify(self)
            },
            // Both applications
            (MonoType::Application(n1, v1), MonoType::Application(n2, v2)) => {
                // Check that type names are the same
                if n1 != n2 { return Err("Could not unify types due to different type functions".to_string()) }
                // Check that lengths are the same
                if v1.len() != v2.len() { return Err("Could not unify types due to different lengths".to_string()) }
                // Empty substitution
                let mut s = sub![];
                // Iterate through v1, v2 and combine
                for (x1, x2) in v1.iter().zip(v2) {
                    s = s.combine(&s.applym(x1).unify(&s.applym(x2))?)
                };
                // Return
                Ok(s)
            }
        }
    }
    pub fn len(&self) -> usize {
        match self {
            MonoType::Application(_, v) => v.len(),
            _ => 0
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeName {
    Int64,
    Char,
    Tuple,
    Bool,
    Function,
}

pub type Context = HashMap<usize, MonoType>;
pub type Substitution = HashMap<usize, MonoType>;

pub trait DoesSub {
    fn applym(&self, m: &MonoType) -> MonoType;
    fn applyc(&self, c: &Context) -> Context;
    fn combine(&self, s: &Substitution) -> Substitution;
}

impl DoesSub for Substitution {
    fn applym(&self, m: &MonoType) -> MonoType {
        match m {
            // Variable: Take mapping if exists, otherwise clone original
            MonoType::Variable(x) => match self.get(x) {
                Some(x) => x.clone(),
                None => m.clone()
            },
            // Applicaton: Apply to all sub-types
            MonoType::Application(name, ls) => MonoType::Application(
                name.clone(), 
                ls.iter().map(|r| self.applym(r)).collect()
            )
        }
    }
    fn applyc(&self, c: &Context) -> Context {
        // New context
        let mut new_c = Context::new();
        // Apply self to each key-value pair
        for (k, v) in c {
            new_c.insert(*k, self.applym(v));
        };
        // Return
        new_c
    }
    fn combine(&self, s: &Substitution) -> Substitution {
        // Copy top-level (s1, self)
        let mut s_new = self.clone();
        // Insert k -> s1(v) for each item (k, v) in s2
        for (k, v) in s { s_new.insert(*k, self.applym(v)); };
        // Return
        s_new
    }
}

pub fn uniqvar() -> usize {
    // Static variable to keep track of unique IDs
    static UID: AtomicUsize = AtomicUsize::new(0);
    // Load value
    let val = UID.load(Ordering::Relaxed);
    // Update value
    UID.store(val + 1, Ordering::Relaxed);
    // Return
    val
}

pub struct TypeSolver {
    var_table: HashMap<String, usize>,
    fn_type_table: HashMap<String, MonoType>
}
impl TypeSolver {
    pub fn new() -> TypeSolver {
        TypeSolver {
            var_table: HashMap::new(),
            fn_type_table: HashMap::new()
        }
    }
    pub fn algw(&mut self, type_env: Context, expr: Expression) -> Result<(Substitution, MonoType), String> {
        match expr {
            Expression::VariableExpression(s) => {
                // Get a unique ID for the variable, create one if does not exist
                let uid = self.var_table.entry(s.clone()).or_insert(uniqvar());
                // Check for variable in the context
                let value: MonoType = match type_env.get(uid) {
                    Some(x) => x.clone(),
                    _ => return Err("Undefined variable: ".to_string() + &s)
                };
                // Return variable
                return Ok((Substitution::new(), value))
            },
            Expression::CallExpression(fname, args) => {
                // Get type of function being called
                let fn_type = match self.fn_type_table.get(&fname) {
                    Some(v) => v.clone(),
                    _ => return Err("Undefined function: ".to_string() + &fname)
                };
                // Get function args types
                let fn_args = match &fn_type {
                    MonoType::Application(TypeName::Function, v) => v.clone(),
                    _ => return Err("Invalid function: ".to_string() + &fname)
                };
                // Verify args length matches function input length
                if args.len() != fn_args.len() - 1 { return Err("Invalid function call: ".to_string() + &fname) }
                // Substitution for learning from args
                let mut args_sub = Substitution::new();
                // Vector for args types
                let mut args_vec = Vec::new();
                // Calculate and verify type of args
                for arg in args {
                    // Get type of argument
                    let (s, t) = self.algw(type_env.clone(), arg.clone())?;
                    // Combine s w/ accumulator substitution
                    args_sub = args_sub.combine(&s);
                    // Add t to args type vec
                    args_vec.push(t);
                }
                // New beta variable
                let beta = MonoType::Variable(uniqvar());
                // Push new type variable to end of args vector as return type
                args_vec.push(beta.clone());
                // Unify function type with learned args types
                let s3 = args_sub.applym(&fn_type).unify(&MonoType::Application(TypeName::Function, args_vec))?;
                // Return function return type as type for this call
                return Ok((s3.combine(&args_sub), s3.applym(&beta)))
            },
            Expression::IntLiteral(_) => {
                // Return fixed type
                return Ok((Substitution::new(), MonoType::Application(TypeName::Int64, Vec::new())))
            },
            Expression::BoolLiteral(_) => {
                // Return fixed type
                return Ok((Substitution::new(), MonoType::Application(TypeName::Bool, Vec::new())))
            },
            Expression::CharLiteral(_) => {
                // Return fixed type
                return Ok((Substitution::new(), MonoType::Application(TypeName::Char, Vec::new())))
            },
            _ => panic!()
        };
    }
}