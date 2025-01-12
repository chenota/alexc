use crate::lexer::lexer::*;

pub struct Parser {
    tokenizer: Lexer
}
impl Parser {
    // Create new parser
    pub fn new(stream: String) -> Parser {
        Parser {
            tokenizer: Lexer::new(stream)
        }
    }
    // Mark position
    pub fn mark(&self) -> usize {
        self.tokenizer.mark()
    }
    // Reset to position
    pub fn reset(&mut self, pos: usize) {
        self.tokenizer.reset(pos)
    }
    // Expect a token
    pub fn expect(&mut self, arg: TokenType) -> Result<Option<&Token>, String> {
        let token = self.tokenizer.peek_token()?;
        if token.0 == arg {
            return Ok(Some(self.tokenizer.get_token()?))
        };
        return Ok(None)
    }
}