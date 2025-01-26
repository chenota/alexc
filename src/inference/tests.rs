#[cfg(test)]
mod inference_tests {
    use crate::{sub, var, app};
    use crate::inference::inference::*;

    #[test]
    fn subm1() {
        // Create substitution
        let s = sub![0 => var!(1), 1 => var!(2)];
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
        let s = sub![0 => var!(1), 1 => var!(2)];
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
        let s = sub![0 => var!(1), 1 => var!(2)];
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
        let s = sub![0 => var!(1), 1 => var!(2)];
        // Type to try on
        let t = app!(TypeName::Tuple => var!(0), var!(1));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Tuple, v) => {
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
        let s = sub![0 => var!(1), 1 => var!(2)];
        // Type to try on
        let t = app!(TypeName::Tuple => var!(1), var!(0));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Tuple, v) => {
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
        let s = sub![0 => var!(1), 1 => var!(2)];
        // Type to try on
        let t = app!(TypeName::Tuple => var!(1), var!(3));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Tuple, v) => {
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
        let s = sub![0 => var!(1), 1 => var!(2)];
        // Type to try on
        let t = app!(TypeName::Tuple => app!(TypeName::Tuple => var!(0)));
        // Apply substitution
        let t_new = s.applym(&t);
        // Check
        match t_new {
            MonoType::Application(TypeName::Tuple, v1) => {
                assert_eq!(v1.len(), 1);
                match &v1[0] {
                    MonoType::Application(TypeName::Tuple, v2) => {
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
        let s1 = sub![1 => var!(2)];
        // Create substitution 2
        let s2 = sub![0 => var!(1)];
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
        let s1 = sub![0 => var!(1)];
        // Create substitution 2
        let s2 = sub![2 => app!(TypeName::Tuple => var!(0))];
        // Combine s1 and s2
        let s3 = s1.combine(&s2);
        // Check
        assert_eq!(s3.len(), 2);
        assert_eq!(*s3.get(&0).unwrap(), MonoType::Variable(1));
        match s3.get(&2).unwrap() {
            MonoType::Application(TypeName::Tuple, v) => {
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
        let s1 = sub![0 => var!(2)];
        // Create substitution 2
        let s2 = sub![0 => var!(1)];
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
        let v1 = MonoType::Variable(uniqvar());
        let v2 = MonoType::Variable(uniqvar());
        let v3 = MonoType::Variable(uniqvar());
        // Check
        assert_eq!(v1, MonoType::Variable(0));
        assert_eq!(v2, MonoType::Variable(1));
        assert_eq!(v3, MonoType::Variable(2));
        // Return
        ()
    }
    #[test]
    fn unif1() {
        // Types to unify
        let t1 = app!(TypeName::Tuple => var!(0), var!(1));
        let t2 = app!(TypeName::Tuple => app!(TypeName::Char), app!(TypeName::Int64));
        // Unify
        let s = t1.unify(&t2).unwrap();
        // Check
        assert_eq!(*s.get(&0).unwrap(), app!(TypeName::Char));
        assert_eq!(*s.get(&1).unwrap(), app!(TypeName::Int64));
        // Return
        ()
    }
    #[test]
    fn unif2() {
        // Types to unify
        let t1 = var!(0);
        let t2 = app!(TypeName::Tuple => app!(TypeName::Char), app!(TypeName::Int64));
        // Unify
        let s = t1.unify(&t2).unwrap();
        // Check
        assert_eq!(*s.get(&0).unwrap(), app!(TypeName::Tuple => app!(TypeName::Char), app!(TypeName::Int64)));
        // Return
        ()
    }
    #[test]
    fn unif3() {
        // Types to unify
        let t1 = app!(TypeName::Char);
        let t2 = app!(TypeName::Int64);
        // Unify and assert is error
        assert!(t1.unify(&t2).is_err());
        // Return
        ()
    }
    #[test]
    fn unif4() {
        // Types to unify
        let t1 = app!(TypeName::Tuple => app!(TypeName::Int64), app!(TypeName::Char));
        let t2 = app!(TypeName::Tuple => app!(TypeName::Char), app!(TypeName::Int64));
        // Unify and assert is error
        assert!(t1.unify(&t2).is_err());
        // Return
        ()
    }
    #[test]
    fn unif5() {
        // Types to unify
        let t1 = var!(0);
        let t2 = app!(TypeName::Tuple => var!(0), var!(0));
        // Unify and assert is error
        assert!(t1.unify(&t2).is_err());
        // Return
        ()
    }
}

#[cfg(test)]
mod typesolver_tests {
    use crate::inference::inference::*;
    use crate::parser::parser::*;
    use crate::{sub, var, app};

    #[test]
    fn litbool() {
        // Expression to determine type of
        let e = Expression::BoolLiteral(false);
        // Empty program
        let p = Parser::new("".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Bool));
        // Return
        ()
    }
    #[test]
    fn litchar() {
        // Expression to determine type of
        let e = Expression::CharLiteral('a');
        // Empty program
        let p = Parser::new("".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Char));
        // Return
        ()
    }
    #[test]
    fn litint() {
        // Expression to determine type of
        let e = Expression::IntLiteral(0);
        // Empty program
        let p = Parser::new("".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Int64));
        // Return
        ()
    }
    #[test]
    fn fn1() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "zero".to_string(),
            vec![]
        );
        // Empty program
        let p = Parser::new("fun zero(): i64 { return 0; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Int64));
        // Return
        ()
    }
    #[test]
    fn fn2() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "ident".to_string(),
            vec![
                Expression::IntLiteral(0)
            ]
        );
        // Empty program
        let p = Parser::new("fun ident(x: i64): i64 { return x; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Int64));
        // Return
        ()
    }
    #[test]
    fn fn3() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "add".to_string(),
            vec![
                Expression::IntLiteral(2),
                Expression::IntLiteral(2)
            ]
        );
        // Empty program
        let p = Parser::new("fun add(x: i64, y: i64): i64 { return x + y; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Int64));
        // Return
        ()
    }
    #[test]
    fn fn4() {
        // Expression to determine type of
        let e: Expression = Expression::CallExpression(
            "idk".to_string(),
            vec![
                Expression::IntLiteral(2)
            ]
        );
        // Empty program
        let p = Parser::new("fun idk(x: i64): bool { return false; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Bool));
        // Return
        ()
    }
    #[test]
    fn fnerr1() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "poop".to_string(),
            vec![
                Expression::IntLiteral(0)
            ]
        );
        // Empty program
        let p = Parser::new("fun ident(x: i64): i64 { return x; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        match tinf.algw(Context::new(), e) {
            Err(_) => (),
            _ => panic!()
        };
        // Return
        ()
    }
    #[test]
    fn fnerr2() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "ident".to_string(),
            vec![
                Expression::IntLiteral(0),
                Expression::IntLiteral(0)
            ]
        );
        // Empty program
        let p = Parser::new("fun ident(x: i64): i64 { return x; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        match tinf.algw(Context::new(), e) {
            Err(_) => (),
            _ => panic!()
        };
        // Return
        ()
    }
    #[test]
    fn fnerr3() {
        // Expression to determine type of
        let e = Expression::CallExpression(
            "ident".to_string(),
            vec![
                Expression::BoolLiteral(false)
            ]
        );
        // Empty program
        let p = Parser::new("fun ident(x: i64): i64 { return x; }".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        match tinf.algw(Context::new(), e) {
            Err(_) => (),
            _ => panic!()
        };
        // Return
        ()
    }
    #[test]
    fn let1() {
        // Expression to determine type of
        let e = Expression::VariableExpression(
            "x".to_string()
        );
        // Empty program
        let p = Parser::new("let x = 0;".to_string()).parse().unwrap();
        // Create type inference object
        let mut tinf = TypeSolver::new(&p).unwrap();
        // Use algorithm w to find type of expression
        let (_, t) = tinf.algw(Context::new(), e).unwrap();
        // Unify and assert is error
        assert_eq!(t, app!(TypeName::Int64));
        // Return
        ()
    }
}