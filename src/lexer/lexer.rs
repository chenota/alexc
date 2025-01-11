use std::usize;

use regex::Regex;

// Turn semicolon-separated list of tokens into rust list
macro_rules! tlist {
    ($($r:literal, $t:path, $f:ident);*) => {
        [ $((concat!("^", $r), $t, $f)),* ]
    }
}

// Token type
#[derive(Clone)]
pub enum TokenType {
    LParen,
    RParen,
    Equal,
    Plus,
    Minus,
    Times,
    Div,
    Semi,
    Identifier,
    Integer,
    LetKw,
    Colon,
    Whitespace,
    Newline,
    EOF,
}

// Token value
pub enum TokenValue {
    Integer(i64),
    String(String),
    Empty
}

// Position token appears in in input (line, col)
pub type Position = (usize, usize);

// All three parts of token
pub type Token = (TokenType, TokenValue, Position);

// Function that generates token value
pub type ValueGenerator = fn(&str) -> Option<TokenValue>;

// Generator functions
fn gen_none(_: &str) -> Option<TokenValue> { None }
fn gen_empty(_: &str) -> Option<TokenValue> { Some(TokenValue::Empty) }
fn gen_int(x: &str) -> Option<TokenValue> { Some(TokenValue::Integer(x.parse().unwrap())) }
fn gen_id(x: &str) -> Option<TokenValue> { Some(TokenValue::String(x.to_string())) }

// Constant tokens list
const TOKENS: [(&str, TokenType, ValueGenerator); 14] = tlist!(
    r"let", TokenType::LetKw, gen_empty;
    r"\+", TokenType::Plus, gen_empty;
    r"-", TokenType::Minus, gen_empty;
    r"\*", TokenType::Times, gen_empty;
    r"/", TokenType::Div, gen_empty;
    r"\(", TokenType::LParen, gen_empty;
    r"\)", TokenType::RParen, gen_empty;
    r"=", TokenType::Equal, gen_empty;
    r";", TokenType::Semi, gen_empty;
    r":", TokenType::Colon, gen_empty;
    r"[0-9]+", TokenType::Integer, gen_int;
    r"[a-zA-Z][a-zA-Z0-9]*", TokenType::Identifier, gen_id;
    r"[\t\v\f\r ]", TokenType::Whitespace, gen_none;
    r"\n", TokenType::Newline, gen_none
);

// Lexer
pub struct Lexer {
    stream: String,
    tokens: Vec<Token>,
    reg: Vec<Regex>,
    pos: usize,
    row: usize,
    col: usize,
}
impl Lexer {
    pub fn new(stream: String) -> Lexer {
        // Compile regular expressions for tokens
        let mut reg = Vec::new();
        for (exstr, _, _) in TOKENS { reg.push(Regex::new(exstr).unwrap()) }
        // Return new lexer object
        Lexer {
            stream,
            tokens: Vec::new(),
            reg,
            pos: 0,
            row: 0,
            col: 0,
        }
    }
    pub fn mark(&self) -> usize {
        self.pos
    }
    pub fn reset(&mut self, pos: usize) {
        self.pos = pos
    }
    pub fn get_token(&mut self) -> Result<&Token, String> {
        // Increment position
        self.pos += 1;
        // Peek token at previous position (or receive error)
        let token = self.peek_token(self.pos - 1);
        // Return result
        return token
    }
    pub fn peek_token(&mut self, pos: usize) -> Result<&Token, String> {
        // If position isn't memoized, generate the next token
        if pos >= self.tokens.len() {
            let next_token = self.gen_next()?;
            self.tokens.push(next_token)
        }
        // Return reference to item in memo
        match self.tokens.get(pos) {
            Some(r) => Ok(r),
            None => Err("Something went wrong in the lexer".to_string())
        }
    }
    fn gen_next(&mut self) -> Result<Token, String> {
        // Return EOF if stream is empty
        if self.stream.len() == 0 { return Ok((TokenType::EOF, TokenValue::Empty, (self.row, self.col))) };
        // Find longest match by testing each available regex, return size of match and index of expression that matched
        let (idx, size): (usize, usize) = self.reg.iter().enumerate().fold(
            // Start w/ zero length, idx doesn't matter
            (0, 0), 
            // Fold function
            |(idx, size), (i, re)| {
                // Test regex
                match re.find(self.stream.as_ref()) {
                    // Found a match, if is longer than current replace current
                    Some(m) => if m.len() > size { (i, m.len()) } else { (idx, size) },
                    // No match, keep going
                    None => (idx, size)
                }
            }
        );
        // Check if longest match is zero and return error if so
        if size == 0 { return Err("Could not generate token at ".to_string() + &self.row.to_string() + ":" + &self.col.to_string()) }
        // Capture old row and column
        let old_row = self.row;
        let old_col = self.col;
        // Update row and column
        match TOKENS[idx].1 {
            TokenType::Newline => { self.row += 1; self.col = 0; },
            _ => { self.col += size }
        };
        // Call value function on matched text (also advance stream forward w/ drain)
        let val = (TOKENS[idx].2)(self.stream.drain(0..size).as_ref());
        // Check if is throwaway or not
        match val {
            Some(val) => Ok((TOKENS[idx].1.clone(), val, (old_row, old_col))),
            None => self.gen_next()
        }
    }
}