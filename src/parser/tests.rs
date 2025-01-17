#[cfg(test)]

mod parser_tests {
    use crate::parser::parser::*;

    #[test]
    fn var1() {
        // Stream
        let s = "x;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::VariableExpression(v)) => assert_eq!(v, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int1() {
        // Stream
        let s = "100;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::IntLiteral(v)) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int2() {
        // Stream
        let s = "(100);".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::IntLiteral(v)) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn plus() {
        // Stream
        let s = "1+1;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop_prec1() {
        // Stream
        let s = "1 + 2 * 3;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop_prec2() {
        // Stream
        let s = "2 * 3 + 1;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::ExprStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn let_stmt() {
        // Stream
        let s = "let x = 0;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::LetStmt((s, None), _) => assert_eq!(s, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn assign_stmt() {
        // Stream
        let s = "x = 0;".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.1[0] {
            Statement::AssignStmt(s, _) => assert_eq!(s, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn fun1() {
        // Stream
        let s = "fun main(): i64 { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Check values
        match &x.0[0] {
            (name, params, ret_type, _) => {
                assert_eq!(name, "main");
                assert_eq!(params.len(), 0);
                match ret_type {
                    MonoType::Function(TypeName::Int64, _) => (),
                    _ => panic!()
                }
            }
        }
        // Return
        ()
    }
}