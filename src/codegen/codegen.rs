use crate::parser::parser::*;

#[derive(Clone)]
pub enum Operand {
    Temporary(usize),
    Variable(String),
    Register(usize),
    Immediate(i32),
    Memory(Box<Operand>, Box<Operand>),
    StackPointer,
    Offset(String)
}
#[derive(Clone)]
pub enum ArithOp {
    Add,
    Sub,
    Div,
    Mul
}
#[derive(Clone)]
pub enum Instruction {
    Label(String),
    Arithetic(ArithOp, Operand, Operand),
    Mov(Operand, Operand),
    Syscall,
    Return
}

pub fn st_lookup(ident: &String, table: &SymbolTable, scope: usize) -> Option<usize> {
    // Check if entry exists in current entry
    match table[scope].1.get(ident) {
        // Exists, return current scope
        Some(_) => Some(scope),
        // Does not exist, return none if in global scope or keep looking otherwise
        _ => if scope == 0 {
            None
        } else {
            st_lookup(ident, table, table[scope].0)
        }
    }
}

pub fn expression_cg(e: &ExpressionBody, reserved: usize, table: &SymbolTable, scope: usize) -> Result<(Vec<Instruction>, usize), String> {
    match e {
        ExpressionBody::BopExpression(op, e1b, e2b) => {
            // Get expressions out of boxes
            let e1 = &e1b.as_ref().0;
            let e2 = &e2b.as_ref().0;
            // Generate code for first operand
            let mut op1code = expression_cg(e1, reserved, table, scope)?;
            // Generate code for second operand, keep in mind # of registers reserved by first operation
            let mut op2code = expression_cg(e2, reserved + op1code.1, table, scope)?;
            // Check operation
            let instr = match op {
                Bop::PlusBop => Instruction::Arithetic(ArithOp::Add, Operand::Temporary(reserved + op1code.1), Operand::Temporary(reserved)),
                Bop::MinusBop => Instruction::Arithetic(ArithOp::Sub, Operand::Temporary(reserved + op1code.1), Operand::Temporary(reserved)),
                Bop::TimesBop => Instruction::Arithetic(ArithOp::Mul, Operand::Temporary(reserved + op1code.1), Operand::Temporary(reserved)),
                Bop::DivBop => Instruction::Arithetic(ArithOp::Div, Operand::Temporary(reserved + op1code.1), Operand::Temporary(reserved)),
            };
            // Put everything together and return
            let mut instrs = Vec::new();
            for x in op1code.0.drain(..) { instrs.push(x) };
            for x in op2code.0.drain(..) { instrs.push(x) };
            instrs.push(instr);
            return Ok((instrs, 1))
        },
        ExpressionBody::IntLiteral(sign, magnitude) => {
            // Move immediate into register
            return Ok((
                vec![ Instruction::Mov(Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32)), Operand::Temporary(reserved)) ],
                1
            ))
        },
        ExpressionBody::VariableExpression(ident) => {
            // Lookup variable in symbol table
            let scope = match st_lookup(ident, table, scope) {
                Some(x) => x,
                None => return Err("Error: Variable".to_string() + ident + " used before definition.")
            };
            // Get variable's location in memory
            let offset = table[scope].1.get(ident).unwrap().1;
            // Load into next available register (mov [sp + offset(ident)] -> tx)
            return Ok((
                vec![ Instruction::Mov(Operand::Memory(Box::new(Operand::StackPointer), Box::new(Operand::Offset(ident.clone()))), Operand::Temporary(reserved)) ], 
                1
            ))
        },
        _ => panic!()
    }
}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable, main: bool) -> Result<Vec<Vec<Instruction>>, String> {
    // Instructions vector
    let mut instrs = vec![ Vec::new() ];
    // Loop through each statement in block
    for stmt in &bl.0 { 
        match &stmt.0 {
            StatementBody::ExprStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression, extend most recent basic block
                for x in expression_cg(e, 0, st, bl.1)?.0.drain(..) {
                    instrs[instrs_len - 1].push(x)
                };
            },
            StatementBody::ReturnStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression, extend most recent basic block
                for x in expression_cg(e, 0, st, bl.1)?.0.drain(..) {
                    instrs[instrs_len - 1].push(x)
                };
                // Special case for main
                if main {
                    // Setup syscall
                    instrs[instrs_len - 1].push(Instruction::Mov(Operand::Register(0), Operand::Immediate(1)));
                    instrs[instrs_len - 1].push(Instruction::Mov(Operand::Register(5), Operand::Temporary(0)));
                    // Do syscall
                    instrs[instrs_len - 1].push(Instruction::Syscall);
                } else {
                    // Push return instruction
                    instrs[instrs_len - 1].push(Instruction::Return);
                }
            }
            _ => panic!()
        }
    };
    // Return
    Ok(instrs)
}