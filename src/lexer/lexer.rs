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
}

pub enum TokenValue {
    Integer(i64),
    String(String),
    None
}

pub type Position = (usize, usize); // row, col

pub type Token = (TokenType, TokenValue, Position);