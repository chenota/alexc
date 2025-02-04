#[cfg(test)]
mod typecheck_tests {
    use crate::typecheck::typecheck::*;
    use crate::parser::parser::{ExpressionBody, Parser};

    #[test]
    fn intord1() {
        assert!(Type::Int(3) < Type::Int(4))
    }
    #[test]
    fn intord2() {
        assert!(Type::Int(5) <= Type::Int(5))
    }
    #[test]
    fn intsynth1() {
        // Stream
        let s = "fun f -> i8 { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Create new type context
        let c = TypeContext::new(&x);
        // Create new expression
        let e = (ExpressionBody::IntLiteral(false, 127), (0, 0));
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(3))
    }
    #[test]
    fn intsynth2() {
        // Stream
        let s = "fun f -> i8 { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Create new type context
        let c = TypeContext::new(&x);
        // Create new expression
        let e = (ExpressionBody::IntLiteral(false, 32766), (0, 0));
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(4))
    }
    #[test]
    fn fcall1() {
        // Stream
        let s = "fun f -> i8 { return 0; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Create new type context
        let c = TypeContext::new(&x);
        // Create new expression
        let e = (
            ExpressionBody::CallExpression("f".to_string(), vec![]), 
            (0, 0)
        );
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(3))
    }
    #[test]
    fn fcall2() {
        // Stream
        let s = "fun f(x: i8) -> i8 { return x; }".to_string();
        // Token generator
        let mut p = Parser::new(s);
        // Parse
        let x = p.parse().unwrap();
        // Create new type context
        let c = TypeContext::new(&x);
        // Create new expression
        let e = (
            ExpressionBody::CallExpression("f".to_string(), vec![ (ExpressionBody::IntLiteral(false, 0), (0, 0)) ]), 
            (0, 0)
        );
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(3))
    }
}