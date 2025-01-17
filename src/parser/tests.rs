#[cfg(test)]

mod parser_tests {
    use crate::parser::parser::*;

    #[test]
    fn var1() {
        // Stream
        let s = "x".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.program().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::VariableExpression(v)) => assert_eq!(v, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
}