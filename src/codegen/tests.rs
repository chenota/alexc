#[cfg(test)]
mod tgen_tests {
    use crate::codegen::codegen::*;
    use crate::parser::parser::*;

    #[test]
    fn expr1() {
        // Stream
        let s = "fun main -> int { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len() - 3, 1);
        match &first[0] {
            Instruction::Mov(op1, op2) => {
                match op1 {
                    Operand::Immediate(0) => (),
                    _ => panic!()
                };
                match op2 {
                    Operand::Temporary(0) => (),
                    _ => panic!()
                }
            }
            _ => panic!()
        };
        // Return
        ()
    }

    #[test]
    fn expr2() {
        // Stream
        let s = "fun main -> int { return 1 + 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len() - 3, 3);
        match &first[0] {
            Instruction::Mov(_, _) => (),
            _ => panic!()
        };
        match &first[1] {
            Instruction::Mov(_, _) => (),
            _ => panic!()
        };
        match &first[2] {
            Instruction::Arithetic(ArithOp::Add, _, _) => (),
            _ => panic!()
        };
        // Return
        ()
    }

    #[test]
    fn expr3() {
        // Stream
        let s = "fun main -> int { return (1 + 1) * (2 + 2); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st, true).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len() - 3, 7);
        let x = match &first[2] {
            Instruction::Arithetic(ArithOp::Add, _, Operand::Temporary(x)) => x,
            _ => panic!()
        };
        let y = match &first[5] {
            Instruction::Arithetic(ArithOp::Add, _, Operand::Temporary(y)) => y,
            _ => panic!()
        };
        let z = match &first[6] {
            Instruction::Arithetic(ArithOp::Mul, _, Operand::Temporary(z)) => z,
            _ => panic!()
        };
        assert_eq!(*x + 1, *y);
        assert_eq!(*x, *z);
        // Return
        ()
    }
}