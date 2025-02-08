#[cfg(test)]

mod parser_tests {
    use crate::parser::parser::*;

    #[test]
    fn var1() {
        // Stream
        let s = "fun main() -> int { return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::VariableExpression(v), _)), _) => assert_eq!(v, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int1() {
        // Stream
        let s = "fun main() -> int { return 100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::IntLiteral(_, v), _)), _) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int2() {
        // Stream
        let s = "fun main() -> int { return (100); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::IntLiteral(_, v), _)), _) => assert_eq!(*v, 100),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn int3() {
        // Stream
        let s = "fun main() -> int { return -100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::IntLiteral(s, v), _)), _) => {
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
        let s = "fun main() -> int { return --100; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::IntLiteral(s, v), _)), _) => {
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
        let s = "fun main() -> int { return 1+1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::PlusBop, _, _), _)), _) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop2() {
        // Stream
        let s = "fun main() -> int { return 1-1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::MinusBop, _, _), _)), _) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop3() {
        // Stream
        let s = "fun main() -> int { return 1 - -1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::MinusBop, _, b), _)), _) => match b.as_ref() {
                (ExpressionBody::IntLiteral(s, v), _) => {
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
        let s = "fun main() -> int { return 0 * 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::TimesBop, b1, b2), _)), _) => match (b1.as_ref(), b2.as_ref()) {
                ((ExpressionBody::IntLiteral(_, v1), _), (ExpressionBody::IntLiteral(_, v2), _)) => {
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
        let s = "fun main() -> int { return 1 + 2 * 3; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::PlusBop, _, _), _)), _) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn bop_prec2() {
        // Stream
        let s = "fun main() -> int { return 2 * 3 + 1; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::BopExpression(Bop::PlusBop, _, _), _)), _) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn let_stmt() {
        // Stream
        let s = "fun main() -> int { let x = 0; return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::LetStmt((s, None), (ExpressionBody::IntLiteral(_, x), _)), _) => {
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
        let s = "fun main() -> int { let x = 0; x = 1; return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[1] {
            (StatementBody::AssignStmt(s, (ExpressionBody::IntLiteral(_, x), _)), _) => {
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
        let s = "fun main() -> int { return -x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::UopExpression(Uop::NegUop, _), _)), _) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn call1() {
        // Stream
        let s = "fun main() -> int { return g(); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::CallExpression(s, args), _)), _) => {
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
        let s = "fun main() -> int { return g(1, 2, 3); }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::CallExpression(s, args), _)), _) => {
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
        let s = "fun main() -> int { return 0 as int; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Get main function from program
        let mainfn = x.get(&"main".to_string()).unwrap();
        // Check values
        match &mainfn.2[0] {
            (StatementBody::ReturnStatement((ExpressionBody::AsExpression(e, t), _)), _) => {
                assert_eq!(*t, Type::Int);
                match e.as_ref() {
                    (ExpressionBody::IntLiteral(_, x), _) => assert_eq!(*x, 0),
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
        let s = "fun main -> int { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        p.parse().unwrap();
        // Return
        ()
    }
}