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
    fn intsynth1() {
        assert_eq!(synth_int(false, 127).unwrap(), Type::Int(3))
    }
    #[test]
    fn intsynth2() {
        assert_eq!(synth_int(true, 32768).unwrap(), Type::Int(4))
    }
}