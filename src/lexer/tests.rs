#[cfg(test)]
mod tgen_tests {
    use crate::lexer::lexer::*;

    #[test]
    fn var1() {
        // Stream
        let s = "x".to_string();
        // Token generator
        let mut t = TokenGen::new(s);
        // Collect tokens
        let seq = t.next().unwrap();
        // Check values
        match seq {
            Ok((TokenType::Identifier, TokenValue::String(s), _)) => assert_eq!(s, "x"),
            _ => panic!()
        }
        // Return
        ()
    }
    #[test]
    fn var2() {
        // Stream
        let s = "X y12 z0Z".to_string();
        // Token generator
        let t = TokenGen::new(s);
        // Collect tokens
        let seq = t
            .take(3)
            .map(|x| match x { Ok((_, TokenValue::String(s), _)) => s, _ => String::new() })
            .collect::<Vec<_>>();
        // Check values
        assert_eq!(seq[0], "X");
        assert_eq!(seq[1], "y12");
        assert_eq!(seq[2], "z0Z");
        // Return
        ()
    }
    #[test]
    fn int1() {
        // Stream
        let s = "12 45 918".to_string();
        // Token generator
        let mut t = TokenGen::new(s);
        // Collect tokens
        let seq = t
            .take(3)
            .map(|x| match x { Ok((_, TokenValue::Integer(s), _)) => s, _ => 0 })
            .collect::<Vec<_>>();
        // Check values
        assert_eq!(seq[0], 12);
        assert_eq!(seq[1], 45);
        assert_eq!(seq[2], 918);
        // Return
        ()
    }
    #[test]
    fn bool1() {
        // Stream
        let s = "false true".to_string();
        // Token generator
        let mut t = TokenGen::new(s);
        // Collect tokens
        let seq = t
            .take(2)
            .map(|x| match x { Ok((_, TokenValue::Bool(s), _)) => s, _ => false })
            .collect::<Vec<_>>();
        // Check values
        assert_eq!(seq[0], false);
        assert_eq!(seq[1], true);
        // Return
        ()
    }
    #[test]
    fn big1() {
        // Stream
        let s = "-12-12".to_string();
        // Token generator
        let t = TokenGen::new(s);
        // Collect tokens
        let seq = t
            .take(5)
            .map(|x| match x { Ok((tt, _, _)) => tt, _ => TokenType::Whitespace })
            .collect::<Vec<_>>();
        // Check values
        assert_eq!(seq[0], TokenType::Minus);
        assert_eq!(seq[1], TokenType::Integer);
        assert_eq!(seq[2], TokenType::Minus);
        assert_eq!(seq[3], TokenType::Integer);
        assert_eq!(seq[4], TokenType::EOF);
        // Return
        ()
    }
    #[test]
    fn big2() {
        // Stream
        let s = "let x = 5;".to_string();
        // Token generator
        let t = TokenGen::new(s);
        // Collect tokens
        let seq = t
            .take(6)
            .map(|x| match x { Ok((tt, _, _)) => tt, _ => TokenType::Whitespace })
            .collect::<Vec<_>>();
        // Check values
        assert_eq!(seq[0], TokenType::LetKw);
        assert_eq!(seq[1], TokenType::Identifier);
        assert_eq!(seq[2], TokenType::Equal);
        assert_eq!(seq[3], TokenType::Integer);
        assert_eq!(seq[4], TokenType::Semi);
        assert_eq!(seq[5], TokenType::EOF);
        // Return
        ()
    }
}

#[cfg(test)]
mod lexer_tests {
    use crate::lexer::lexer::*;

    #[test]
    fn markreset() {
        // Stream
        let s = "x".to_string();
        // Lexer
        let mut l = Lexer::new(s);
        // Mark
        assert_eq!(l.mark(), 0);
        // Reset
        l.reset(1);
        // Mark again
        assert_eq!(l.mark(), 1);
        // Return
        ()
    }
    #[test]
    fn get() {
        // Stream
        let s = "x".to_string();
        // Lexer
        let mut l = Lexer::new(s);
        // Next token
        let t = l.get_token();
        // Check token
        match t { Ok((tt, _, _)) => assert_eq!(*tt, TokenType::Identifier), _ => panic!() }
        // Check mark
        assert_eq!(l.mark(), 1);
        // Return
        ()
    }
}