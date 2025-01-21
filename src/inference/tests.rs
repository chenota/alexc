#[cfg(test)]
mod inference_tests {
    use crate::{sub, var, app};
    use crate::inference::inference::*;

    #[test]
    fn subm1() {
        // Create substitution
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = var!(0);
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
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = var!(1);
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
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = var!(3);
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
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = app!(TypeName::Char => var!(0), var!(1));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Char, v) => {
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
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = app!(TypeName::Char => var!(1), var!(0));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Char, v) => {
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
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = app!(TypeName::Char => var!(1), var!(3));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Char, v) => {
                assert_eq!(v.len(), 2);
                assert_eq!(v[0], MonoType::Variable(2));
                assert_eq!(v[1], MonoType::Variable(3));
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn subfnested() {
        // Create substitution
        let s = sub![0 -> var!(1), 1 -> var!(2)];
        // Type to try on
        let t = app!(TypeName::Char => app!(TypeName::Int64 => var!(0)));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Char, v1) => {
                assert_eq!(v1.len(), 1);
                match &v1[0] {
                    MonoType::Application(TypeName::Int64, v2) => {
                        assert_eq!(v2.len(), 1);
                        assert_eq!(v2[0], MonoType::Variable(1))
                    },
                    _ => panic!()
                }
            },
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn combine1() {
        // Create substitution 1
        let s1 = sub![1 -> var!(2)];
        // Create substitution 2
        let s2 = sub![0 -> var!(1)];
        // Combine s1 and s2
        let s3 = s1.combine(&s2);
        // Check
        assert_eq!(s3.len(), 2);
        assert_eq!(*s3.get(&0).unwrap(), MonoType::Variable(2));
        assert_eq!(*s3.get(&1).unwrap(), MonoType::Variable(2));
        // Return
        ()
    }
    #[test]
    fn combine2() {
        // Create substitution 1
        let s1 = sub![0 -> var!(1)];
        // Create substitution 2
        let s2 = sub![2 -> app!(TypeName::Char => var!(0))];
        // Combine s1 and s2
        let s3 = s1.combine(&s2);
        // Check
        assert_eq!(s3.len(), 2);
        assert_eq!(*s3.get(&0).unwrap(), MonoType::Variable(1));
        match s3.get(&2).unwrap() {
            MonoType::Application(TypeName::Char, v) => {
                assert_eq!(v.len(), 1);
                assert_eq!(v[0], MonoType::Variable(1));
            },
            _ => panic!()
        };
        // Return
        ()
    }
    #[test]
    fn combine3() {
        // Create substitution 1
        let s1 = sub![0 -> var!(2)];
        // Create substitution 2
        let s2 = sub![0 -> var!(1)];
        // Combine s1 and s2
        let s3 = s1.combine(&s2);
        // Check
        assert_eq!(s3.len(), 1);
        assert_eq!(*s3.get(&0).unwrap(), MonoType::Variable(1));
        // Return
        ()
    }
    #[test]
    fn uniq() {
        // Get some unique type variables
        let v1 = uniqvar();
        let v2 = uniqvar();
        let v3 = uniqvar();
        // Check
        assert_eq!(v1, MonoType::Variable(0));
        assert_eq!(v2, MonoType::Variable(1));
        assert_eq!(v3, MonoType::Variable(2));
        // Return
        ()
    }
}