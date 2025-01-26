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
            Statement::ReturnStatement(Expression::IntLiteral(_, v)) => assert_eq!(*v, 100),
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
            Statement::ReturnStatement(Expression::IntLiteral(_, v)) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int3() {
        // Stream
        let s = "fun main() -> i8 { return -100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::IntLiteral(s, v)) => {
                assert_eq!(*v, 100);
                assert!(s)
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int4() {
        // Stream
        let s = "fun main() -> i8 { return --100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::IntLiteral(s, v)) => {
                assert_eq!(*v, 100);
                assert!(!s)
            },
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
    fn bop2() {
        // Stream
        let s = "fun main() -> i8 { return 1-1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::MinusBop, _, _)) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop3() {
        // Stream
        let s = "fun main() -> i8 { return 1 - -1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::MinusBop, _, b)) => match b.as_ref() {
                Expression::IntLiteral(s, v) => {
                    assert!(*s);
                    assert_eq!(*v, 1);
                },
                _ => panic!()
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop4() {
        // Stream
        let s = "fun main() -> i8 { return 0 * 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::BopExpression(Bop::TimesBop, b1, b2)) => match (b1.as_ref(), b2.as_ref()) {
                (Expression::IntLiteral(_, v1), Expression::IntLiteral(_, v2)) => {
                    assert_eq!(*v1, 0);
                    assert_eq!(*v2, 1);
                },
                _ => panic!()
            },
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
            Statement::LetStmt((s, None), Expression::IntLiteral(_, x)) => {
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
            Statement::AssignStmt(s, Expression::IntLiteral(_, x)) => {
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
    #[test]
    fn as_expr() {
        // Stream
        let s = "fun main() -> i8 { return 0 as i8; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            Statement::ReturnStatement(Expression::AsExpression(e, t)) => {
                assert_eq!(*t, Type::Int(3));
                match e.as_ref() {
                    Expression::IntLiteral(_, x) => assert_eq!(*x, 0),
                    _ => panic!()
                }
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn fn_no_paren() {
        // Stream
        let s = "fun main -> i8 { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        p.parse().unwrap();
        // Return
        ()
    }
}