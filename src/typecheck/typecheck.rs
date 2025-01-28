use std::cmp::{PartialOrd, Ordering};
use std::usize;

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

fn synth_int(sign: bool, magnitude: usize) -> Result<Type, String> {
    // Return OK Int by default
    Ok(Type::Int(if sign {
        if magnitude <= (i8::MAX as usize) { 3 }
        else if magnitude <= (i16::MAX as usize) { 4 }
        else if magnitude <= (i32::MAX as usize) { 5 }
        else if magnitude <= (i64::MAX as usize) { 6 }
        else { return Err("Int literal too large".to_string()) }
    } else {
        if magnitude <= (i8::MIN.abs() as usize) { 3 }
        else if magnitude <= (i16::MAX.abs() as usize) { 4 }
        else if magnitude <= (i32::MAX.abs() as usize) { 5 }
        else if magnitude <= (i64::MAX.abs() as usize) { 6 }
        else { return Err("Int literal too large".to_string()) }
    }))
}