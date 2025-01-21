pub enum MonoType {
    Variable(usize),
    Function(TypeName, Vec<MonoType>)
}

pub enum TypeName {
    Int64,
    Char
}