use crate::parser::parser::*;

pub enum Operand {
    Temporary(usize),
    Variable(String),
    Register(usize),
    Immediate(i32)
}
pub enum ArithOp {
    Add,
    Sub,
    Div,
    Mul
}
pub enum Instruction {
    Label(String),
    Arithetic(ArithOp, Operand, Operand),
    Mov(Operand, Operand)
}

pub fn expression_cg(e: &ExpressionBody, reserved: usize) -> Result<(Vec<Instruction>, usize), String> {
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
            return Ok((
                vec![ Instruction::Mov(Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32)), Operand::Temporary(reserved)) ],
                1
            ))
        },
        _ => panic!()
    }
}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable) -> Result<Vec<Vec<Instruction>>, String> {
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
            _ => panic!()
        }
    };
    // Return
    Ok(instrs)
}