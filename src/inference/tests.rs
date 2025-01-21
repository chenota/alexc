#[cfg(test)]
mod inference_tests {
    use crate::inference::inference::*;

    #[test]
    fn subm1() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Variable(0);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Variable(1) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subm2() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Variable(1);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Variable(2) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subm3() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Variable(3);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Variable(3) => (),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subf1() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Function(TypeName::Char, vec![MonoType::Variable(0), MonoType::Variable(1)]);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Function(TypeName::Char, v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], MonoType::Variable(1));
                assert_eq!(v[1], MonoType::Variable(2));
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subf2() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Function(TypeName::Char, vec![MonoType::Variable(1), MonoType::Variable(0)]);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Function(TypeName::Char, v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], MonoType::Variable(2));
                assert_eq!(v[1], MonoType::Variable(1));
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subf3() {
        // Create substitution
        let mut s = Substitution::new();
        s.insert(0, MonoType::Variable(1));
        s.insert(1, MonoType::Variable(2));
        // Type to try on
        let t = MonoType::Function(TypeName::Char, vec![MonoType::Variable(1), MonoType::Variable(3)]);
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Function(TypeName::Char, v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], MonoType::Variable(2));
                assert_eq!(v[1], MonoType::Variable(3));
            },
            _ => panic!()
        }
        // Return
        ()
    }
}