use crate::lexer::lexer::*;

pub type Program = (Vec<Function>, Vec<Statement>);
pub type Function = (Ident, IdentList, StmtList);

pub enum Statement {
    ExprStatement(Expression),
    ReturnStatement(Expression),
    LetStmt(Ident, Option<Ident>, Expression)
}

pub enum Expression {
    UopExpression(Uop, Box<Expression>),
    BopExpression(Bop, Box<Expression>, Box<Expression>),
    LiteralExpression(Literal),
    VariableExpression(Ident),
    CallExpression(Ident, ExprList)
}

pub enum Uop {
    NegUop
}
pub enum Bop {
    PlusBop,
    MinusBop,
    TimesBop,
    DivBop
}
pub enum Literal {
    Integer(i64),
    String(String),
    Boolean(bool),
    Character(u8)
}

pub type Ident = String;

pub type IdentList = Vec<Ident>;
pub type StmtList = Vec<Statement>;
pub type ExprList = Vec<Expression>;

pub struct Parser {
    tokenizer: Lexer,
    row: usize,
    col: usize
}
impl Parser {
    // Create new parser
    pub fn new(stream: String) -> Parser {
        Parser {
            tokenizer: Lexer::new(stream),
            row: 0,
            col: 0
        }
    }
    // Invalid token error message
    fn invalid_token_err(&self) -> String {
        "Syntax Error: Invalid token ".to_string() + " at " + &self.row.to_string() + ":" + &self.col.to_string()
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
        // Peek current token
        let token = self.tokenizer.peek_token()?;
        // If token matches, return some
        if token.0 == arg {
            // Update row and col
            self.row = token.2.0;
            self.col = token.2.1;
            // Return and advance
            return Ok(Some(self.tokenizer.get_token()?))
        };
        // No match, return none
        Ok(None)
    }
    // Expect a token
    pub fn expect_err(&mut self, arg: TokenType) -> Result<&Token, String> {
        // Peek current token
        let token = self.tokenizer.peek_token()?;
        // If token matches, return some
        if token.0 == arg {
            // Update row and col
            self.row = token.2.0;
            self.col = token.2.1;
            // Return and advance
            return Ok(self.tokenizer.get_token()?)
        };
        // No match, return error message
        Err("Syntax Error: Expected token ".to_string() + &arg.to_string() + " at " + &self.row.to_string() + ":" + &self.col.to_string())
    }
    // Start parsing
    pub fn start(&mut self) -> Result<Program, String> {
        // Reset pos
        self.reset(0);
        // Parse program
        let result = self.program()?;
        // Check for eof
        self.expect_err(TokenType::EOF)?;
        // Return result
        Ok(result)
    }
    fn program(&mut self) -> Result<Program, String> {
        // Vectors to hold parts of the program
        let mut fns = Vec::new();
        let mut stmts = Vec::new();
        // Capture statements and functions until can't anymore
        loop {
            // Check if function
            match self.function()? {
                Some(f) => {
                    fns.push(f);
                    continue
                },
                None => ()
            };
            // Check if statement
            match self.statement()? {
                Some(s) => {
                    stmts.push(s);
                    continue
                },
                None => ()
            };
            // Neither matched, return
            break
        }
        // Return program
        Ok((fns, stmts))
    }
    fn function(&mut self) -> Result<Option<Function>, String> {
        // Check for function keyword
        if self.expect(TokenType::FunKw)?.is_none() { return Ok(None) }
        // Extract ident
        let id = match self.expect_err(TokenType::Identifier)? {
            (_, TokenValue::String(s), _) => s.clone(),
            _ => return Err(self.invalid_token_err())
        };
        // Expect open paren
        self.expect_err(TokenType::LParen)?;
        // Parse identifier list
        let idlist = self.identlist()?;
        // Expect closing paren
        self.expect_err(TokenType::RParen)?;
        // Expect opening bracket
        self.expect_err(TokenType::LBracket)?;
        // Parse a statement list
        let stmts = self.stmtlist()?;
        // Expect closing bracket
        self.expect_err(TokenType::RBracket)?;
        // Return
        Ok(Some((id, idlist, stmts)))
    }
    fn statement(&mut self) -> Result<Option<Statement>, String> {

    }
    fn expression(&mut self) -> Result<Option<Expression>, String> {

    }
    fn stmtlist(&mut self) -> Result<StmtList, String> {
        // Statements
        let mut stmts = Vec::new();
        // Loop
        loop {
            // Attempt to parse a statement
            match self.statement()? {
                Some(st) => stmts.push(st),
                _ => break
            }
        };
        // Return
        Ok(stmts)
    }
    fn identlist(&mut self) -> Result<IdentList, String> {
        // Identifiers
        let mut idents = Vec::new();
        // Loop
        loop {
            // Attempt to parse an identifier
            match self.expect(TokenType::Identifier)? {
                Some((_, TokenValue::String(s), _)) => {
                    // Push found identifier string
                    idents.push(s.clone());
                    // Break if no comma
                    if self.expect(TokenType::Comma)?.is_none() { break }
                },
                _ => break
            };
        };
        // Return the list
        Ok(idents)
    }
}