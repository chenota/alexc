#[cfg(test)]
mod typecheck_tests {
    use crate::typecheck::typecheck::*;

    #[test]
    fn intord1() {
        assert!(Type::Int(3) < Type::Int(4))
    }
    #[test]
    fn intord2() {
        assert!(Type::Int(5) <= Type::Int(5))
    }
    #[test]
    fn intsynth() {
        assert_eq!(synth_int(false, 250).unwrap(), Type::Int(3))
    }
}