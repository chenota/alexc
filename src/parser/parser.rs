use crate::lexer::lexer::*;

pub type Program = (Vec<Function>, Vec<Statement>);
pub type Function = (Ident, IdentList, StmtList);

pub enum Statement {
    ExprStatement(Expression),
    ReturnStatement(Expression),
    LetStmt(TypedIdent, Expression),
    AssignStmt(Ident, Expression),
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
pub type TypedIdent = (String, Option<String>);

pub type IdentList = Vec<Ident>;
pub type TypedIdentList = Vec<TypedIdent>;
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
    // Expected message
    fn expected_err(&self, x: &str) -> String {
        "Syntax Error: Expected ".to_string() + x + " at " + &self.row.to_string() + ":" + &self.col.to_string()
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
            _ => return Err(self.expected_err("identifier"))
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
        // Mark position
        let pos = self.mark();
        // Skip to next next token for lookahead
        self.tokenizer.get_token()?;
        // Check if next is an equal sign (in case ... = ... if so)
        if !self.expect(TokenType::Equal)?.is_none() {
            // Expect an identifier
            let id = match self.expect_err(TokenType::Identifier)? {
                (_, TokenValue::String(s), _) => s.clone(),
                _ => return Err(self.expected_err("identifier"))
            };
            // Expect an equal sign
            self.expect_err(TokenType::Equal)?;
            // Parse an expression
            let ex = match self.expression()? {
                Some(e) => e,
                None => return Err(self.expected_err("expression"))
            };
            // Put together and return
            return Ok(Some(Statement::AssignStmt(id, ex)))
        };
        // Reset position
        self.reset(pos);
        // Check for let token
        if !self.expect(TokenType::LetKw)?.is_none() {
            // Parse a typed identifier
            let tid = match self.typed_ident()? {
                Some(x) => x,
                None => return Err(self.expected_err("typed identifier"))
            };
            // Expect an equal sign
            self.expect_err(TokenType::Equal)?;
            // Parse an expression
            let e = match self.expression()? {
                Some(e) => e,
                None => return Err(self.expected_err("expression"))
            };
            // Put together and return
            return Ok(Some(Statement::LetStmt(tid, e)))
        };
        // Attempt to parse expression
        match self.expression()? {
            Some(e) => Ok(Some(Statement::ExprStatement(e))),
            None => {
                // Reset position
                self.reset(pos);
                // Return None
                Ok(None)
            }
        }
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
    fn typed_ident(&mut self) -> Result<Option<TypedIdent>, String> {
        // Mark position
        let pos = self.mark();
        // Expect an identifier, return none if not found
        let id = match self.expect(TokenType::Identifier)? {
            Some((_, TokenValue::String(s), _)) => s.clone(),
            _ => { self.reset(pos); return Ok(None) }
        };
        // Expect a colon
        let t = match self.expect(TokenType::Colon)? {
            // Found colon, parse type name
            Some(_) => {
                // Expect err an identifier following the colon
                let t_name = match self.expect_err(TokenType::Identifier)? {
                    (_, TokenValue::String(s), _) => s.clone(),
                    _ => return Err(self.expected_err("identifier"))
                };
                // Some of type name
                Some(t_name)
            },
            // No colon, no type name provided
            _ => None
        };
        // Put together
        Ok(Some((id, t)))
    }
}