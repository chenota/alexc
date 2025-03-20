#[cfg(test)]
mod tgen_tests {
    use crate::codegen::codegen::*;
    use crate::parser::parser::*;
    use std::collections::HashMap;

    #[test]
    fn ir_expr1() {
        // Stream
        let s = "fun main -> int { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true, None, &HashMap::new()).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len(), 1);
        match &first[0] {
            IRInstruction::Exit(op1) => {
                match op1 {
                    Operand::Immediate(0) => (),
                    _ => panic!()
                };
            }
            _ => panic!()
        };
        // Return
        ()
    }

    #[test]
    fn ir_expr2() {
        // Stream
        let s = "fun main -> int { return 1 + 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true, None, &HashMap::new()).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len(), 2);
        match &first[0] {
            IRInstruction::Arithmetic(ArithOp::Add, Operand::Immediate(_), Operand::Immediate(_)) => (),
            _ => panic!()
        };
        match &first[1] {
            IRInstruction::Exit(op1) => {
                match op1 {
                    Operand::Temporary(_) => (),
                    _ => panic!()
                }
            },
            _ => panic!()
        };
        // Return
        ()
    }

    #[test]
    fn ir_expr3() {
        // Stream
        let s = "fun main -> int { return (1 + 1) * (2 + 2); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true, None, &HashMap::new()).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len(), 4);
        match &first[0] {
            IRInstruction::Arithmetic(ArithOp::Add, Operand::Immediate(_), Operand::Immediate(_)) => (),
            _ => panic!()
        };
        match &first[1] {
            IRInstruction::Arithmetic(ArithOp::Add, Operand::Immediate(_), Operand::Immediate(_)) => (),
            _ => panic!()
        };
        match &first[2] {
            IRInstruction::Arithmetic(ArithOp::Mul, Operand::Temporary(_), Operand::Temporary(_)) => (),
            _ => panic!()
        };
        match &first[3] {
            IRInstruction::Exit(Operand::Temporary(_)) => (),
            _ => panic!()
        };
        // Return
        ()
    }
}