use std::cmp::{PartialOrd, Ordering};

#[derive(Clone)]
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