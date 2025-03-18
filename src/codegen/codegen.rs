use crate::parser::parser::*;

pub enum Operand {

}
pub enum Instruction {
    Label(String)
}

pub fn expression_cg(e: &ExpressionBody) -> Result<Vec<Instruction>, String> {

}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable) -> Result<Vec<Vec<Instruction>>, String> {
    // Instructions vector
    let mut instrs = Vec::new();
    // Loop through each statement in block
    for stmt in bl.0 {
        match &stmt.0 {
            StatementBody::ExprStatement((e, _)) =>
        }
    }
}