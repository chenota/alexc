use crate::parser::parser::*;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::collections::HashMap;


pub type FunctionTable = HashMap<String, (ForceTypedIdentList, Type)>;

#[derive(Clone)]
pub enum Operand {
    Temporary(usize),
    Immediate(i32),
    Variable(String),
    Parameter(String),
    Return
}
impl ToString for Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Temporary(x) => "t".to_string() + &x.to_string(),
            Operand::Immediate(x) => "#".to_string() + &x.to_string(),
            Operand::Variable(s) => s.clone(),
            Operand::Parameter(s) => "p(".to_string() + s + ")",
            Operand::Return => "ret".to_string()
        }
    }
}
#[derive(Clone)]
pub enum ArithOp {
    Add,
    Sub,
    Div,
    Mul
}
impl ToString for ArithOp {
    fn to_string(&self) -> String {
        match self {
            ArithOp::Sub => "sub".to_string(),
            ArithOp::Add => "add".to_string(),
            ArithOp::Mul => "mul".to_string(),
            ArithOp::Div => "div".to_string()
        }
    }
}
#[derive(Clone)]
pub enum IRInstruction {
    Label(String),
    Arithmetic(ArithOp, Operand, Operand),
    Mov(Operand, Operand),
    Return,
    Exit(Operand),
    Call(String),
}
impl ToString for IRInstruction {
    fn to_string(&self) -> String {
        match self {
            IRInstruction::Arithmetic(op, op1, op2) => op.to_string() + " " + &op1.to_string() + " " + &op2.to_string(),
            IRInstruction::Exit(op1) => "exit ".to_string() + &op1.to_string(),
            IRInstruction::Label(s) => s.clone() + ":",
            IRInstruction::Mov(op1, op2) => "mov ".to_string() + &op1.to_string() + " " + &op2.to_string(),
            IRInstruction::Return => "return".to_string(),
            IRInstruction::Call(s) => "call _".to_string() + s
        }
    }
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

pub fn expression_cg(e: &ExpressionBody, reserved: usize, st: &SymbolTable, scope: usize, ft: &FunctionTable) -> Result<(Vec<IRInstruction>, usize, Operand), String> {
    match e {
        ExpressionBody::BopExpression(op, e1b, e2b) => {
            // Get expressions out of boxes
            let e1 = &e1b.as_ref().0;
            let e2 = &e2b.as_ref().0;
            // Generate code for first operand
            let mut op1code = expression_cg(e1, reserved, st, scope, ft)?;
            // Peek at the next variable, skip codegen if atomic
            let (mut op2inst, op2rt) = match &e2 {
                ExpressionBody::IntLiteral(sign, magnitude) => (Vec::new(), Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32))),
                ExpressionBody::VariableExpression(s) => (Vec::new(), Operand::Variable(s.clone())),
                _ => {
                    // Generate code for second operand, keep in mind # of registers reserved by first operation
                    let op2code = expression_cg(e2, reserved + op1code.1, st, scope, ft)?;
                    (op2code.0, op2code.2)
                }
            };
            // Check operation
            let instr = match op {
                Bop::PlusBop => IRInstruction::Arithmetic(ArithOp::Add, op2rt, op1code.2),
                Bop::MinusBop => IRInstruction::Arithmetic(ArithOp::Sub, op2rt, op1code.2),
                Bop::TimesBop => IRInstruction::Arithmetic(ArithOp::Mul, op2rt, op1code.2),
                Bop::DivBop => IRInstruction::Arithmetic(ArithOp::Div, op2rt, op1code.2),
            };
            // Put everything together and return
            let mut instrs = Vec::new();
            for x in op1code.0.drain(..) { instrs.push(x) };
            for x in op2inst.drain(..) { instrs.push(x) };
            instrs.push(instr);
            return Ok((instrs, 1, Operand::Temporary(reserved)))
        },
        ExpressionBody::IntLiteral(sign, magnitude) => {
            // Move immediate into register
            return Ok((
                vec![IRInstruction::Mov(Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32)), Operand::Temporary(reserved))],
                1,
                Operand::Temporary(reserved)
            ))
        },
        ExpressionBody::VariableExpression(ident) => {
            // Load into next available register (mov [sp + offset(ident)] -> tx)
            return Ok((
                vec![IRInstruction::Mov(Operand::Variable(ident.clone()), Operand::Temporary(reserved))], 
                1,
                Operand::Temporary(reserved)
            ))
        },
        ExpressionBody::CallExpression(fname, alist) => {
            // Lookup function
            let finfo = match ft.get(fname) {
                Some(x) => x,
                None => return Err("Invalid function call".to_string())
            };
            // Check params = args
            if alist.len() != finfo.0.len() { return Err("Invalid function call".to_string()); };
            // Empty list for instructions
            let mut instrs = Vec::new();
            // Generate code for parameters, load into parameter slots
            for (i, (e, _)) in alist.iter().enumerate() {
                // Generate code
                let (mut code, _, operand) = expression_cg(e, reserved, st, scope, ft)?;
                // Move code to instrs list
                for instr in code.drain(..) { instrs.push(instr) };
                // Move result into parameter slot
                instrs.push(IRInstruction::Mov(operand, Operand::Parameter(finfo.0[i].0.clone())))
            };
            // Jump to label for function
            instrs.push(IRInstruction::Call(fname.clone()));
            // Return list
            return Ok((
                instrs, 
                0,
                Operand::Return
            ));
        },
        _ => panic!()
    }
}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable, main: bool, passthrough: Option<Vec<IRInstruction>>, ft: &FunctionTable) -> Result<Vec<Vec<IRInstruction>>, String> {
    // Instructions vector
    let mut instrs = Vec::new();
    // Handle passthrough
    instrs.push(match passthrough {
        Some(v) => v,
        _ => Vec::new()
    });
    // Loop through each statement in block
    for stmt in &bl.0 { 
        match &stmt.0 {
            StatementBody::ExprStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression, extend most recent basic block
                for x in expression_cg(e, 0, st, bl.1, ft)?.0.drain(..) {
                    instrs[instrs_len - 1].push(x)
                };
            },
            StatementBody::ReturnStatement((e, _)) => {
                // Get length of instructions
                let instrs_len = instrs.len();
                // Generate code for expression
                let (mut code, _, operand) = expression_cg(e, 0, st, bl.1, ft)?;
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

pub fn program_to_ir(prog: Program) -> Result<(Vec<Vec<IRInstruction>>, SymbolTable), String> {
    // Make prog mutable
    let (funs, mut st) = prog;
    // Construct function table from program
    let mut ft = FunctionTable::new();
    for fun in &funs { ft.insert(fun.0.clone(), (fun.1.0.clone(), fun.1.1.clone())); };
    // Blocks vector
    let mut blocks = Vec::new();
    // Loop through functions in the program
    for fun in funs {
        // Start function block
        let header = vec![
            IRInstruction::Label("_".to_string() + &fun.0),
        ];
        // Generate blocks for that function
        let mut fun_blocks = basic_blocks(&fun.1.2, &mut st, fun.0 == "main", Some(header), &ft)?;
        // Add blocks to blocks vector
        for block in fun_blocks.drain(..) { blocks.push(block) }
    };
    // Return
    Ok((blocks, st))
}

pub fn ir_to_file(ir: Vec<Vec<IRInstruction>>, path: String) -> Result<(), String> {
    // Open file
    let mut file = match OpenOptions::new()
        .write(true)
        .truncate(true)
        .open(path) {
        Ok(f) => f,
        Err(e) => return Err(e.to_string())
    };
    // Write lines to file
    for block in ir {
        for instr in block {
            if let Err(e) = writeln!(file, "{}", instr.to_string()) {
                return Err(e.to_string());
            }
        }
    };
    Ok(())
}