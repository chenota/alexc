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

pub fn synth_int(sign: bool, magnitude: usize) -> Result<Type, String> {
    // Return OK Int by default
    Ok(Type::Int(
        // If number is positive
        if !sign {
            // Check smallest type that fits
            if magnitude <= (i8::MAX as usize) { 3 }
            else if magnitude <= (i16::MAX as usize) { 4 }
            else if magnitude <= (i32::MAX as usize) { 5 }
            else if magnitude <= (i64::MAX as usize) { 6 }
            else { return Err("Int literal too large".to_string()) }
        } 
        // Number is negative
        else {
            // Check largest negative type that fits
            // Since 2's compliment, largest negative = largest positive + 1
            if magnitude <= (i8::MAX as usize + 1) { 3 }
            else if magnitude <= (i16::MAX as usize + 1) { 4 }
            else if magnitude <= (i32::MAX as usize + 1) { 5 }
            else if magnitude <= (i64::MAX as usize + 1) { 6 }
            else { return Err("Int literal too large".to_string()) }
        }
    ))
}