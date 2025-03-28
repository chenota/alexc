use crate::parser::parser::*;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize,Ordering};

pub type FunctionTable = HashMap<String, (ForceTypedIdentList, Type)>;

#[derive(Clone)]
pub enum Operand {
    Temporary(usize),
    Immediate(i32),
    Variable(String),
    Parameter(usize),
    Return
}
impl ToString for Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Temporary(x) => "t".to_string() + &x.to_string(),
            Operand::Parameter(x) => "p".to_string() + &x.to_string(),
            Operand::Immediate(x) => "#".to_string() + &x.to_string(),
            Operand::Variable(s) => s.clone(),
            Operand::Return => "ret".to_string()
        }
    }
}
impl PartialEq for Operand {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Operand::Immediate(x), Operand::Immediate(y)) => x == y,
            (Operand::Parameter(x), Operand::Parameter(y)) => x == y,
            (Operand::Return, Operand::Return) => true,
            (Operand::Temporary(x), Operand::Temporary(y)) => x == y,
            (Operand::Variable(x), Operand::Variable(y)) => x == y,
            _ => false
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
    Move(Operand, Operand),
    Return,
    Exit(Operand),
    Call(String),
    PushScope(usize),
    PopScope(usize),
    JumpIfZero(Operand, String),
    Jump(String),
    Declare(String)
}
impl ToString for IRInstruction {
    fn to_string(&self) -> String {
        match self {
            IRInstruction::Arithmetic(op, op1, op2) => op.to_string() + " " + &op1.to_string() + " " + &op2.to_string(),
            IRInstruction::Exit(op1) => "exit ".to_string() + &op1.to_string(),
            IRInstruction::Label(s) => "label ".to_string() + &s.clone(),
            IRInstruction::Move(op1, op2) => "move ".to_string() + &op1.to_string() + " " + &op2.to_string(),
            IRInstruction::Return => "return".to_string(),
            IRInstruction::Call(s) => "call _".to_string() + s,
            IRInstruction::PushScope(x) => "scope ".to_string() + &x.to_string(),
            IRInstruction::PopScope(x) => "unscope ".to_string() + &x.to_string(),
            IRInstruction::JumpIfZero(op, s) => "jeqz ".to_string() + &op.to_string() + " " + s,
            IRInstruction::Jump(s) => "jump ".to_string() + s,
            IRInstruction::Declare(s) => "declare ".to_string() + s,
        }
    }
}

pub fn expression_cg(e: &ExpressionBody, reserved: usize, target: Option<Operand>, ft: &FunctionTable) -> Result<(Vec<IRInstruction>, usize, Operand), String> {
    match e {
        ExpressionBody::BopExpression(op, e1b, e2b) => {
            // Get expressions out of boxes
            let e1 = &e1b.as_ref().0;
            let e2 = &e2b.as_ref().0;
            // Generate code for first operand
            let mut op1code = expression_cg(e1, reserved, target, ft)?;
            // Peek at the next variable, skip codegen if atomic
            let (mut op2inst, op2res, op2rt) = match &e2 {
                ExpressionBody::IntLiteral(sign, magnitude) => (Vec::new(), 0, Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32))),
                ExpressionBody::VariableExpression(s) => (Vec::new(), 0, Operand::Variable(s.clone())),
                _ => {
                    // Generate code for second operand, keep in mind # of registers reserved by first operation
                    let op2code = expression_cg(e2, reserved + op1code.1,None, ft)?;
                    (op2code.0, op2code.1, op2code.2)
                }
            };
            // Operation 1 return (mutable since might have to change)
            let mut op1rt = op1code.2.clone();
            // Hold instruction order
            let mut instrs = Vec::new();
            // Push operation 1's instructions to list
            for x in op1code.0.drain(..) { instrs.push(x) };
            // If both op1 and op2 return to same address, move op1 to temporary
            let moved = op1rt == op2rt;
            if moved {
                // Register to move op1 into register just past op2
                let op1reg = Operand::Temporary(reserved + op2res);
                // Generate move instruction
                instrs.push(IRInstruction::Move(op1rt, op1reg.clone()));
                // Switch return address of op1
                op1rt = op1reg;
            };
            // Add op2 instructions to list
            for x in op2inst.drain(..) { instrs.push(x) };
            // Push binary operation instruction
            instrs.push(match op {
                Bop::PlusBop => IRInstruction::Arithmetic(ArithOp::Add, op2rt.clone(), op1rt.clone()),
                Bop::MinusBop => IRInstruction::Arithmetic(ArithOp::Sub, op2rt.clone(), op1rt.clone()),
                Bop::TimesBop => IRInstruction::Arithmetic(ArithOp::Mul, op2rt.clone(), op1rt.clone()),
                Bop::DivBop => IRInstruction::Arithmetic(ArithOp::Div, op2rt.clone(), op1rt.clone()),
            });
            // If needed to shift registers, shift result to op2's return register
            if moved {
                instrs.push(IRInstruction::Move(op1rt, op2rt))
            };
            return Ok((instrs, 1, op1code.2))
        },
        ExpressionBody::IntLiteral(sign, magnitude) => {
            // Move immediate into register
            return Ok((
                vec![IRInstruction::Move(Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32)), match &target { Some(t) => t.clone(), _ => Operand::Temporary(reserved) })],
                match &target { Some(_) => 0, _ => 1 },
                match target { Some(t) => t, _ => Operand::Temporary(reserved) }
            ))
        },
        ExpressionBody::VariableExpression(ident) => {
            // Check if has target and is same as variable
            match &target {
                Some(t) => if *t != Operand::Variable(ident.clone()) {
                    // Load into next available register (mov [sp + offset(ident)] -> tx)
                    return Ok((
                        vec![IRInstruction::Move(Operand::Variable(ident.clone()), match &target { Some(t) => t.clone(), _ => Operand::Temporary(reserved) })], 
                        match &target { Some(_) => 0, _ => 1 },
                        match target { Some(t) => t, _ => Operand::Temporary(reserved) }
                    ))
                },
                _ => ()
            };
            return Ok((Vec::new(),0,Operand::Variable(ident.clone())))
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
                // Operand to push to call register
                let operand = match e {
                    ExpressionBody::IntLiteral(sign, magnitude) => {
                        // Literal operand
                        Operand::Immediate((if *sign {-1} else {1}) * (*magnitude as i32))
                    },
                    ExpressionBody::VariableExpression(s) => {
                        // Variable operand
                        Operand::Variable(s.clone())
                    },
                    _ => {
                        // Generate code
                        let (mut code, _, operand) = expression_cg(e, reserved, None, ft)?;
                        // Move code to instrs list
                        for instr in code.drain(..) { instrs.push(instr) };
                        // Return operand
                        operand
                    }
                };
                // Move result into parameter slot
                instrs.push(IRInstruction::Move(operand, Operand::Parameter(i)))
            };
            // Jump to label for function
            instrs.push(IRInstruction::Call(fname.clone()));
            // If targeting a register that's not return register, generate mov instruction
            match &target {
                Some(r) => if *r != Operand::Return {
                    instrs.push(IRInstruction::Move(Operand::Return, r.clone()))
                },
                _ => ()
            };
            // Return list
            return Ok((
                instrs, 
                0,
                match target { Some(r) => r, _ => Operand::Return }
            ));
        },
        _ => panic!()
    }
}

pub fn basic_blocks(bl: &Block, st: &mut SymbolTable, main: bool, passthrough: Option<Vec<IRInstruction>>, ft: &FunctionTable) -> Result<Vec<Vec<IRInstruction>>, String> {
    // Unique label counter
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    // Instructions vector
    let mut instrs = Vec::new();
    // Handle passthrough
    instrs.push(match passthrough {
        Some(v) => v,
        _ => Vec::new()
    });
    // Push stack
    instrs.last_mut().unwrap().push(IRInstruction::PushScope(bl.1));
    // Flag to pop stack at very end of block
    let mut fpop: bool = true;
    // Loop through each statement in block
    for stmt in &bl.0 { 
        match &stmt.0 {
            StatementBody::ExprStatement((e, _)) => {
                // Generate code for expression, extend most recent basic block
                for x in expression_cg(e, 0, None, ft)?.0.drain(..) {
                    instrs.last_mut().unwrap().push(x)
                };
            },
            StatementBody::ReturnStatement((e, _)) => {
                // Generate code for expression
                let (mut code, _, operand) = expression_cg(e, 0, None, ft)?;
                for x in code.drain(..) {
                    instrs.last_mut().unwrap().push(x)
                };
                // Pop stack
                instrs.last_mut().unwrap().push(IRInstruction::PopScope(bl.1));
                fpop = false;
                // Special case for main
                if main {
                    // Do exit
                    instrs.last_mut().unwrap().push(IRInstruction::Exit(operand));
                } else {
                    // Push return instruction
                    instrs.last_mut().unwrap().push(IRInstruction::Return);
                }
            },
            StatementBody::LetStmt((id, ty), e) => {
                // Check that type is an int
                match ty {
                    Some(Type::Int) => (),
                    _ => return Err("Type must be an int".to_string())
                };
                // Generate declare instruction
                instrs.last_mut().unwrap().push(IRInstruction::Declare(id.clone()));
                // Generate instructions for expressions
                let (mut code, _, _) = expression_cg(&e.0, 0, Some(Operand::Variable(id.clone())), ft)?;
                for x in code.drain(..) {
                    instrs.last_mut().unwrap().push(x);
                }
            },
            StatementBody::AssignStmt(id, e) => {
                // Generate instructions for expressions
                let (mut code, _, _) = expression_cg(&e.0, 0, Some(Operand::Variable(id.clone())), ft)?;
                for x in code.drain(..) {
                    instrs.last_mut().unwrap().push(x);
                }
            },
            StatementBody::BlockStmt(bl2) => {
                // Generate instructions for block
                let mut newinstrs = basic_blocks(bl2, st, main, None, ft)?;
                // Push first generated basic block onto existing basic block (guaranteed to return at least one)
                for x in newinstrs.drain(0..1).next().unwrap() {
                    instrs.last_mut().unwrap().push(x)
                };
                // Push the rest of the basic blocks onto new vectors
                for x in newinstrs {
                    instrs.push(x)
                }
            },
            StatementBody::IfStmt(e, b1, b2) => {
                // Generate code for condition
                let (mut code, _, operand) = expression_cg(&e.0, 0, None, ft)?;
                for x in code.drain(..) {
                    instrs.last_mut().unwrap().push(x);
                }
                // Labels for if statement
                let endlabel = "_end".to_string() + &COUNTER.load(Ordering::Relaxed).to_string();
                let faillabel = "_fail".to_string() + &COUNTER.load(Ordering::Relaxed).to_string();
                // Increment reserved counter
                COUNTER.store(COUNTER.load(Ordering::Relaxed) + 1, Ordering::Relaxed);
                // Add branch instruction
                instrs.last_mut().unwrap().push(IRInstruction::JumpIfZero(operand, if b2.is_none() { endlabel.clone() } else { faillabel.clone() }));
                // Generate code for take condition
                let mut tblocks = basic_blocks(b1, st, main, None, ft)?;
                // Push first generated basic block onto existing basic block (guaranteed to return at least one)
                for x in tblocks.drain(0..1).next().unwrap() {
                    instrs.last_mut().unwrap().push(x)
                };
                // Push the rest of the basic blocks onto new vectors
                for x in tblocks {
                    instrs.push(x)
                }
                // Jump to end label if fail code follows take code
                if b2.is_some() { instrs.last_mut().unwrap().push(IRInstruction::Jump(endlabel.clone())) };
                // Generate code for fail condition if need to
                match b2 {
                    Some(b2) => {
                        // New basic block
                        instrs.push(Vec::new());
                        // Push fail label onto instrs
                        instrs.last_mut().unwrap().push(IRInstruction::Label(faillabel));
                        // Generate code blocks
                        let mut fblocks = basic_blocks(b2, st, main, None, ft)?;
                        // Push first generated basic block onto existing basic block (guaranteed to return at least one)
                        for x in fblocks.drain(0..1).next().unwrap() {
                            instrs.last_mut().unwrap().push(x)
                        };
                        // Push the rest of the basic blocks onto new vectors
                        for x in fblocks {
                            instrs.push(x)
                        }
                    },
                    None => ()
                };
                // New basic block
                instrs.push(Vec::new());
                // End label
                instrs.last_mut().unwrap().push(IRInstruction::Label(endlabel));
            },
            StatementBody::WhileStmt(condition, body) => {
                // Generate labels
                let loopstart = "_loopstart".to_string() + &COUNTER.load(Ordering::Relaxed).to_string();
                let loopend = "_loopend".to_string() + &COUNTER.load(Ordering::Relaxed).to_string();
                COUNTER.store(COUNTER.load(Ordering::Relaxed) + 1, Ordering::Relaxed);
                // Make a new basic block
                instrs.push(Vec::new());
                // Push start label onto the block
                instrs.last_mut().unwrap().push(IRInstruction::Label(loopstart.clone()));
                // Generate condition checking code
                let (mut cond_instrs, _, operand) = expression_cg(&condition.0, 0, None, ft)?;
                // Push condition code
                for instr in cond_instrs.drain(..) { instrs.last_mut().unwrap().push(instr) }
                // Jump out of loop if condition is zero
                instrs.last_mut().unwrap().push(IRInstruction::JumpIfZero(operand, loopend.clone()));
                // Generate code for loop body
                let mut lblocks = basic_blocks(body, st, main, None, ft)?;
                // Push first generated basic block onto existing basic block (guaranteed to return at least one)
                for x in lblocks.drain(0..1).next().unwrap() { instrs.last_mut().unwrap().push(x) };
                // Push the rest of the basic blocks onto new vectors
                for x in lblocks { instrs.push(x) };
                // Jump back to loop start
                instrs.last_mut().unwrap().push(IRInstruction::Jump(loopstart));
                // New basic block
                instrs.push(Vec::new());
                // Add loopend label
                instrs.last_mut().unwrap().push(IRInstruction::Label(loopend));
            }
        }
    };
    // End of block; pop stack
    if fpop { instrs.last_mut().unwrap().push(IRInstruction::PopScope(bl.1)) };
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
        .create(true)
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

#[derive(Clone)]
pub enum X86Operand {
    Register(usize),
    Immediate(i32)
}
impl ToString for X86Operand {
    fn to_string(&self) -> String {
        match self {
            X86Operand::Immediate(x) => "$".to_string() + &x.to_string(),
            X86Operand::Register(x) => match *x {
                0 => "rax".to_string(),
                1 => "rcx".to_string(),
                2 => "rdx".to_string(),
                3 => "rbx".to_string(),
                4 => "rsi".to_string(),
                5 => "rdi".to_string(),
                6 => "rsp".to_string(),
                7 => "rbp".to_string(),
                _ => x.to_string()
            }
        }
    }
}

#[derive(Clone)]
pub enum X86Instruction {
    Global,
    Label(String),
    Move(X86Operand, X86Operand)
}
impl ToString for X86Instruction {
    fn to_string(&self) -> String {
        match self {
            X86Instruction::Label(s) => s.clone() + ":",
            X86Instruction::Move(o1, o2) => "mov ".to_string() + &o1.to_string() + " " + &o2.to_string(),
            X86Instruction::Global => "global _main".to_string()
        }
    }
}
#[derive(Clone)]
pub enum RegisterValue {
    Variable(String),
    Temporary(usize)
}
pub type RegisterTable = Vec<Vec<RegisterValue>>;
pub type TemporaryTable = HashMap<usize, (Option<usize>, Option<usize>)>;

pub fn st_lookup(ident: &String, table: &SymbolTable, scope: usize) -> Option<usize> {
    // Check if entry exists in current entry
    match table[scope].1.get(ident) {
        // Exists, return current scope if is valid
        Some((_, _, valid, _)) => if *valid {
            Some(scope)
        } else {
            st_lookup(ident, table, table[scope].0)
        },
        // Does not exist, return none if in global scope or keep looking otherwise
        _ => if scope == 0 {
            None
        } else {
            st_lookup(ident, table, table[scope].0)
        }
    }
}

pub fn rank_registers(st: &SymbolTable, scope: usize, tt: &TemporaryTable, rt: &RegisterTable) -> Vec<usize> {
    // Look through each register
    rt.iter().map(|r| {
        // Initialize score to zero
        let mut score = 0;
        // For each value the register stores...
        for value in r {
            // Check out the value
            match value {
                // Holding a variable value
                RegisterValue::Variable(ident) => {
                    // Get scope of variable (should always be in scope)
                    let scope = st_lookup(ident, st, scope).unwrap();
                    // Figure out where value is stored
                    match st[scope].1.get(ident).unwrap().3.1 {
                        // In memory
                        Some(_) => score += 1,
                        // Not in memory
                        None => score += 2
                    }
                },
                // Holding a temporary value
                RegisterValue::Temporary(x) => match tt.get(x).unwrap().1 {
                    // In memory
                    Some(_) => score += 1,
                    // Not in memory
                    None => score += 2
                }
            }
        };
        // Return score for that register
        score
    }).collect()
}

pub fn best_register(rankings: &Vec<usize>, restrict: Vec<usize>) -> usize {
    // Store best register
    let mut best_register_score: usize = std::usize::MAX;
    // Iterate through rankings
    for (i, ranking) in rankings.iter().enumerate() {
        // Only consider unrestricted registers
        if !restrict.contains(&i) {
            // Update score
            best_register_score = best_register_score.min(*ranking);
        }
    };
    // Return index of register with best score
    rankings.iter().enumerate().position(|(i,r)| !restrict.contains(&i) && *r == best_register_score).unwrap()
}

pub fn ralloc(register: usize, st: &mut SymbolTable, scope: usize, tt: &mut TemporaryTable, rt: &mut RegisterTable) -> Vec<X86Instruction> {
    // Instructions needed to perform allocation
    let mut instrs = Vec::new();
    // Check which values this register stores
    for value in rt.get(register).unwrap() {
        // Check which kind of value
        match value {
            RegisterValue::Variable(ident) => {
                // Get scope of variable (should always be in scope)
                let scope = st_lookup(&ident, st, scope).unwrap();
                // Invalidate register entry for variable
                st.get_mut(scope).unwrap().1.get_mut(ident).unwrap().3.0 = None;
                // Figure out where is stored
                match st[scope].1.get(ident).unwrap().3.1 {
                    // In memory (don't do anything)
                    Some(_) => (),
                    // Not in memory (TODO allocate to stack)
                    None => panic!()
                }
            },
            RegisterValue::Temporary(x) => {
                // Invalidate register entry for variable
                tt.get_mut(&x).unwrap().0 = None;
                // Figure out where is stored
                match tt.get(&x).unwrap().1 {
                    // In memory (don't do anything)
                    Some(_) => (),
                    // Not in memory (TODO allocate to stack)
                    None => panic!()
                }
            }
        }
    };
    // Clear register table entry for this register
    rt[register].clear();
    // Return
    instrs
}

pub fn bb_to_x86(bb: Vec<IRInstruction>, st: &mut SymbolTable) -> Result<Vec<X86Instruction>, String> {
    // Generate register table for this basic block
    let mut rt: RegisterTable = Vec::new();
    for _ in 0..16 { rt.push(Vec::new()) };
    // Generate temporary table for this basic block
    let mut tt: TemporaryTable = HashMap::new();
    // Empty instructions vector
    let mut instrs = Vec::new();
    // Iterate through each IR instruction in the basic block
    for instr in bb {
        match instr {
            IRInstruction::Label(s) => {
                instrs.push(X86Instruction::Label(s))
            },
            IRInstruction::Move(o1, o2) => {
                // Generate operands for move
                let (ox1, ox2) = match o1 {
                    Operand::Immediate(x) => match o2 {
                        Operand::Temporary(y) => {
                            // Get temporary table entry for this value
                            let entry = tt.entry(y).or_insert((None, None)).clone();
                            // Check where value exists
                            match entry {
                                // Already in a register
                                (Some(r), _) => {
                                    (X86Operand::Immediate(x), X86Operand::Register(r))
                                },
                                // Not in a register
                                _ => {
                                    // Invalidate memory value if exists
                                    tt.get_mut(&y).unwrap().1 = None;
                                    // Rank registers (TODO attach scope to basic blocks)
                                    let rankings = rank_registers(st, 0, &tt, &rt);
                                    // Get register with smallest ranking
                                    let selected_register = best_register(&rankings, Vec::new());
                                    // Allocate selected register (TODO attach scope)
                                    for instr in ralloc(selected_register, st, 0, &mut tt, &mut rt) { instrs.push(instr) };
                                    // Return selected register
                                    (X86Operand::Immediate(x), X86Operand::Register(selected_register))
                                }
                            }
                        }
                        _ => panic!()
                    },
                    _ => panic!()
                };
                // Generate move instruction
                instrs.push(X86Instruction::Move(ox1, ox2))
            },
            _ => ()
        }
    };
    // Return
    Ok(instrs)
}

pub fn ir_to_x86(ir: Vec<Vec<IRInstruction>>, st: SymbolTable) -> Result<Vec<X86Instruction>, String> {
    // Make symbol table mutable
    let mut st = st;
    // Empty instructions vector
    let mut instrs = vec![
        X86Instruction::Global
    ];
    // Add instructions for each basic block
    for bb in ir {
        for instr in bb_to_x86(bb, &mut st)? {
            instrs.push(instr)
        }
    };
    // Return
    Ok(instrs)
}

pub fn x86_to_file(x86: Vec<X86Instruction>, path: String) -> Result<(), String> {
    // Open file
    let mut file = match OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path) {
        Ok(f) => f,
        Err(e) => return Err(e.to_string())
    };
    // Write lines to file
    for instr in x86 {
        if let Err(e) = writeln!(file, "{}", instr.to_string()) {
            return Err(e.to_string());
        }
    }
    Ok(())
}