#[cfg(test)]
mod typecheck_tests {
    use crate::typecheck::typecheck::*;
    use crate::parser::parser::ExpressionBody;

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
        // Create new type context
        let c = TypeContext::new();
        // Create new expression
        let e = (ExpressionBody::IntLiteral(false, 127), (0, 0));
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(3))
    }
    #[test]
    fn intsynth2() {
        // Create new type context
        let c = TypeContext::new();
        // Create new expression
        let e = (ExpressionBody::IntLiteral(false, 32766), (0, 0));
        // Sytnthesize type
        assert_eq!(synth_type(&c, &e).unwrap(), Type::Int(4))
    }
}