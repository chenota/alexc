#[cfg(test)]

mod parser_tests {
    use crate::parser::parser::*;
    use crate::typecheck::typecheck::*;

    #[test]
    fn var1() {
        // Stream
        let s = "fun main() -> i8 { return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::VariableExpression(v)) => assert_eq!(v, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int1() {
        // Stream
        let s = "fun main() -> i8 { return 100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::IntLiteral(v)) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int2() {
        // Stream
        let s = "fun main() -> i8 { return (100); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::IntLiteral(v)) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop1() {
        // Stream
        let s = "fun main() -> i8 { return 1+1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop_prec1() {
        // Stream
        let s = "fun main() -> i8 { return 1 + 2 * 3; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop_prec2() {
        // Stream
        let s = "fun main() -> i8 { return 2 * 3 + 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::PlusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn let_stmt() {
        // Stream
        let s = "fun main() -> i8 { let x = 0; return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::LetStmt((s, None), Expression::IntLiteral(x)) => {
                assert_eq!(*x, 0);
                assert_eq!(s, "x")
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn assign_stmt() {
        // Stream
        let s = "fun main() -> i8 { let x = 0; x = 1; return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[1] {
            Statement::AssignStmt(s, Expression::IntLiteral(x)) => {
                assert_eq!(*x, 1);
                assert_eq!(s, "x")
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn uop1() {
        // Stream
        let s = "fun main() -> i8 { return -x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::UopExpression(Uop::NegUop, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn call1() {
        // Stream
        let s = "fun main() -> i8 { return g(); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::CallExpression(s, args)) => {
                assert_eq!(s, "g");
                assert_eq!(args.len(), 0)
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn call2() {
        // Stream
        let s = "fun main() -> i8 { return g(1, 2, 3); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::CallExpression(s, args)) => {
                assert_eq!(s, "g");
                assert_eq!(args.len(), 3)
            },
            _ => panic!()
        }
        // Return
        ()
    }
}