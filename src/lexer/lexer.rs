macro_rules! tlist {
    ($($r:literal, $t:path, $f:ident);*) => {
        [ $((concat!("^", $r), $t, $f)),* ]
    }
}

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
}

pub enum TokenValue {
    Integer(i64),
    String(String),
    Empty
}

pub type Position = (usize, usize); // row, col

pub type Token = (TokenType, TokenValue, Position);

pub type ValueGenerator = fn(&str) -> Option<TokenValue>;

fn gen_none(_: &str) -> Option<TokenValue> { None }
fn gen_empty(_: &str) -> Option<TokenValue> { Some(TokenValue::Empty) }
fn gen_int(x: &str) -> Option<TokenValue> { Some(TokenValue::Integer(x.parse().unwrap())) }
fn gen_id(x: &str) -> Option<TokenValue> { Some(TokenValue::String(x.to_string())) }

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