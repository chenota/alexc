use crate::parser::parser::*;

#[derive(Clone)]
pub enum Operand {
    Temporary(usize),
    Register(usize),
    Immediate(i32),
    Variable(String),
    Return
}
#[derive(Clone)]
pub enum ArithOp {
    Add,
    Sub,
    Div,
    Mul
}
#[derive(Clone)]
pub enum IRInstruction {
    Label(String),
    Arithetic(ArithOp, Operand, Operand),
    Mov(Operand, Operand),
    Return,
    Exit(Operand)
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

pub fn expression_cg(e: &ExpressionBody, reserved: usize) -> Result<(Vec<IRInstruction>, usize, Operand), String> {
    match e {
        ExpressionBody::BopExpression(op, e1b, e2b) => {
            // Get expressions out of boxes
            let e1 = &e1b.as_ref().0;
            let e2 = &e2b.as_ref().0;
            // Generate code for first operand
            let mut op1code = expression_cg(e1, reserved)?;
            // Generate code for second operand, keep in mind # of registers reserved by first operation
            let mut op2code = expression_cg(e2, reserved + op1code.1)?;
            // Check operation
            let instr = match op {
                Bop::PlusBop => IRInstruction::Arithetic(ArithOp::Add, op2code.2, op1code.2),
                Bop::MinusBop => IRInstruction::Arithetic(ArithOp::Sub, op2code.2, op1code.2),
                Bop::TimesBop => IRInstruction::Arithetic(ArithOp::Mul, op2code.2, op1code.2),
                Bop::DivBop => IRInstruction::Arithetic(ArithOp::Div, op2code.2, op1code.2),
            };
            // Put everything together and return
            let mut instrs = Vec::new();
            for x in op1code.0.drain(..) { instrs.push(x) };
            for x in op2code.0.drain(..) { instrs.push(x) };
            instrs.push(instr);
            return Ok((instrs, 1, Operand::Temporary(reserved)))
        },
        ExpressionBody::IntLiteral(sign, magnitude) => {
            // Move immediate into register
            return Ok((
                Vec::new(),
                0,
                Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32))
            ))
        },
        ExpressionBody::VariableExpression(ident) => {
            // Load into next available register (mov [sp + offset(ident)] -> tx)
            return Ok((
                Vec::new(), 
                0,
                Operand::Variable(ident.clone())
            ))
        },
        _ => panic!()
    }
}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable, main: bool) -> Result<Vec<Vec<IRInstruction>>, String> {
    // Instructions vector
    let mut instrs = vec![ Vec::new() ];
    // Loop through each statement in block
    for stmt in &bl.0 { 
        match &stmt.0 {
            StatementBody::ExprStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression, extend most recent basic block
                for x in expression_cg(e, 0)?.0.drain(..) {
                    instrs[instrs_len - 1].push(x)
                };
            },
            StatementBody::ReturnStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression
                let (mut code, _, operand) = expression_cg(e, 0)?;
                for x in code.drain(..) {
                    instrs[instrs_len - 1].push(x)
                };
                // Special case for main
                if main {
                    // Do exit
                    instrs[instrs_len - 1].push(IRInstruction::Exit(operand));
                } else {
                    // Move result into return register
                    instrs[instrs_len - 1].push(IRInstruction::Mov(operand, Operand::Return));
                    // Push return instruction
                    instrs[instrs_len - 1].push(IRInstruction::Return);
                }
            }
            _ => panic!()
        }
    };
    // Return
    Ok(instrs)
}