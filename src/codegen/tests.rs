#[cfg(test)]
mod tgen_tests {
    use crate::codegen::codegen::*;
    use crate::parser::parser::*;

    #[test]
    fn expr1() {
        // Stream
        let s = "fun main -> int { 0; return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let (fns, mut st) = p.parse().unwrap();
        // Get main function
        let main = fns.get("main").unwrap();
        // Do codegen
        let cg_result = basic_blocks(&main.2, &mut st).unwrap();
        // Get first basic block of function
        let first = cg_result[0].clone();
        // Test codegen result
        assert_eq!(first.len(), 1);
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
}