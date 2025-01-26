use crate::lexer::lexer::*;
use crate::typecheck::typecheck::*;
use std::collections::HashMap;

pub type Program = (HashMap<Ident, Function>);
pub type Function = (ForceTypedIdentList, Type, StmtList);

pub enum Statement {
    ExprStatement(Expression),
    ReturnStatement(Expression),
    LetStmt(TypedIdent, Expression),
    AssignStmt(Ident, Expression),
}

#[derive(Clone)]
pub enum Expression {
    UopExpression(Uop, Box<Expression>),
    BopExpression(Bop, Box<Expression>, Box<Expression>),
    IntLiteral(bool, usize),
    VariableExpression(Ident),
    CallExpression(Ident, ExprList)
}

#[derive(Clone)]
pub enum Uop {
    NegUop
}

#[derive(Clone)]
pub enum Bop {
    PlusBop,
    MinusBop,
    TimesBop,
    DivBop
}

pub type Ident = String;
pub type TypedIdent = (String, Option<Type>);
pub type ForceTypedIdent = (String, Type);

pub type IdentList = Vec<Ident>;
pub type TypedIdentList = Vec<TypedIdent>;
pub type ForceTypedIdentList = Vec<ForceTypedIdent>;
pub type StmtList = Vec<Statement>;
pub type ExprList = Vec<Expression>;

const ARITH_PLUS: [(TokenType, Bop); 2] = [
    (TokenType::Plus, Bop::PlusBop),
    (TokenType::Minus, Bop::MinusBop),
];

const ARITH_TIMES: [(TokenType, Bop); 2] = [
    (TokenType::Times, Bop::TimesBop),
    (TokenType::Div, Bop::DivBop),
];

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
    // Generic error message w/ position
    fn generic_err(&self, x: &str) -> String {
        "Syntax Error at ".to_string() + &self.row.to_string() + ":" + &self.col.to_string() + ": " + x
    }
    // Mark position
    fn mark(&self) -> usize {
        self.tokenizer.mark()
    }
    // Reset to position
    fn reset(&mut self, pos: usize) {
        self.tokenizer.reset(pos)
    }
    // Expect a token
    fn expect(&mut self, arg: TokenType) -> Result<Option<&Token>, String> {
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
    fn expect_err(&mut self, arg: TokenType) -> Result<&Token, String> {
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
    pub fn parse(&mut self) -> Result<Program, String> {
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
        let mut fns = HashMap::new();
        // Capture statements and functions until can't anymore
        loop {
            // Check if function
            match self.function()? {
                Some(f) => {
                    fns.insert(f.0, f.1);
                    continue
                },
                None => ()
            };
            // Didn't match, return
            break
        }
        // Return program
        Ok(fns)
    }
    fn function(&mut self) -> Result<Option<(String, Function)>, String> {
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
        let idlist = self.force_typed_identlist()?;
        // Expect closing paren
        self.expect_err(TokenType::RParen)?;
        // Expect Arrow
        self.expect_err(TokenType::Arrow)?;
        // Expect ident
        let ret_type = self.parse_type()?;
        // Expect opening bracket
        self.expect_err(TokenType::LBracket)?;
        // Parse a statement list
        let stmts = self.stmtlist()?;
        // Expect closing bracket
        self.expect_err(TokenType::RBracket)?;
        // Return
        Ok(Some((id, (idlist, ret_type, stmts))))
    }
    fn statement(&mut self) -> Result<Option<Statement>, String> {
        // Mark position
        let pos = self.mark();
        // Skip to next next token for lookahead
        self.tokenizer.get_token()?;
        // Check if next is an equal sign (in case ... = ... if so)
        if self.expect(TokenType::Equal)?.is_some() {
            // Reset pos
            self.reset(pos);
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
            // Expect a semicolon
            self.expect_err(TokenType::Semi)?;
            // Put together and return
            return Ok(Some(Statement::AssignStmt(id, ex)))
        };
        // Reset position
        self.reset(pos);
        // Check for let token
        if self.expect(TokenType::LetKw)?.is_some() {
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
            // Expect a semicolon
            self.expect_err(TokenType::Semi)?;
            // Put together and return
            return Ok(Some(Statement::LetStmt(tid, e)))
        };
        // Check for return keyword
        if self.expect(TokenType::ReturnKw)?.is_some() {
            // Parse an expression
            match self.expression()? {
                Some(e) => {
                    // Expect a semicolon
                    self.expect_err(TokenType::Semi)?;
                    // Return
                    return Ok(Some(Statement::ReturnStatement(e)))
                },
                None => return Err(self.expected_err("Expression"))
            }
        };
        // Attempt to parse expression
        match self.expression()? {
            Some(e) => {
                // Expect a semicolon
                self.expect_err(TokenType::Semi)?;
                // Return
                Ok(Some(Statement::ExprStatement(e)))
            },
            None => {
                // Reset position
                self.reset(pos);
                // Return None
                Ok(None)
            }
        }
    }
    fn parse_bop_expr(&mut self, f: fn(&mut Self) -> Result<Option<Expression>, String>, bops: &[(TokenType, Bop)]) -> Result<Option<Expression>, String> {
        // Parse expression
        let head = match f(self)? {
            Some(e) => e,
            None => return Ok(None)
        };
        // Create list of (bop, expr) pairs
        let mut elist = Vec::new();
        // Consume <bop>, <expression> pairs
        loop {
            // Check for one of the bops
            match bops.iter().fold(
                None, 
                |acc, bop | {
                    match acc { 
                        Some(_) => acc, 
                        None => match self.expect(bop.0.clone()) { Ok(Some(_)) => Some(bop.1.clone()), _ => None }
                    }
                }
            ) {
                Some(b) => {
                    // Consume addtl expression
                    let e1 = match f(self)? {
                        Some(e) => e,
                        None => return Err(self.expected_err("expression"))
                    };
                    // Push to list
                    elist.push((b, e1))
                },
                // Nothing found, stop
                None => break
            }
        };
        // Create left-associative list and return
        Ok(Some(elist.drain(..).fold(
            head,
            |acc, bop| Expression::BopExpression(bop.0, Box::new(acc), Box::new(bop.1))
        )))
    }
    fn expression(&mut self) -> Result<Option<Expression>, String> {
        self.parse_bop_expr(Self::e1, &ARITH_PLUS)
    }
    fn e1(&mut self) -> Result<Option<Expression>, String> {
        self.parse_bop_expr(Self::e2, &ARITH_TIMES)
    }
    fn e2(&mut self) -> Result<Option<Expression>, String> {
        // Check for negative sign
        if self.expect(TokenType::Minus)?.is_some() {
            // Parse expression
            return match self.expression()? {
                Some(e) => {
                    // Check type of expression
                    match e {
                        // Int literal, flip sign
                        Expression::IntLiteral(sign, magnitude) => Ok(Some(Expression::IntLiteral(!sign, magnitude))),
                        // Anything else, create uop expression
                        _ => Ok(Some(Expression::UopExpression(Uop::NegUop, Box::new(e))))
                    }
                },
                None => Err(self.expected_err("Expression"))
            };
        };
        // Parse e3
        self.e3()
    }
    fn e3(&mut self) -> Result<Option<Expression>, String> {
        // Parse value, return none if not found
        let v = match self.value()? {
            Some(v) => v,
            _ => return Ok(None)
        };
        // Check for open paren
        if self.expect(TokenType::LParen)?.is_some() {
            // Check if e is an identifier
            let vstr = match v {
                Expression::VariableExpression(s) => s,
                _ => return Err("Syntax Error: Called a non-function expression".to_string())
            };
            // Parse expression list
            let elist = self.exprlist()?;
            // Expect right paren
            self.expect_err(TokenType::RParen)?;
            // Return
            return Ok(Some(Expression::CallExpression(vstr, elist)))
        };
        // Return value
        Ok(Some(v))
    }
    fn value(&mut self) -> Result<Option<Expression>, String> {
        // Check for parenthesis
        if self.expect(TokenType::LParen)?.is_some() {
            // Parse expression
            let e = match self.expression()? {
                Some(e) => e,
                None => return Err(self.expected_err("Expression"))
            };
            // Expect rparen
            self.expect_err(TokenType::RParen)?;
            // Return e
            return Ok(Some(e));
        };
        // Check for ident
        match self.expect(TokenType::Identifier)? {
            Some((_, TokenValue::String(x), _)) => return Ok(Some(Expression::VariableExpression(x.clone()))),
            _ => ()
        };
        // Check for integer literal
        match self.expect(TokenType::Integer)? {
            Some((_, TokenValue::Integer(x), _)) => return Ok(Some(Expression::IntLiteral(false, x.clone()))),
            _ => ()
        };
        Ok(None)
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
                let t_name = self.parse_type()?;
                // Some of type name
                Some(t_name)
            },
            // No colon, no type name provided
            _ => None
        };
        // Put together
        Ok(Some((id, t)))
    }
    fn force_typed_ident(&mut self) -> Result<Option<ForceTypedIdent>, String> {
        // Parse a regular typed ident
        let tid = self.typed_ident()?;
        // Check if none or found
        match tid {
            Some((s, Some(t))) => Ok(Some((s, t))),
            Some(_) => Err(self.generic_err("Function definitions must be explicitly type-annotated")),
            None => Ok(None)
        }
    }
    fn typed_identlist(&mut self) -> Result<TypedIdentList, String> {
        // Identifiers
        let mut idents = Vec::new();
        // Loop
        loop {
            // Attempt to parse an identifier
            match self.typed_ident()? {
                Some(tid) => {
                    // Push found identifier string
                    idents.push(tid);
                    // Break if no comma
                    if self.expect(TokenType::Comma)?.is_none() { break }
                },
                _ => break
            };
        };
        // Return the list
        Ok(idents)
    }
    fn force_typed_identlist(&mut self) -> Result<ForceTypedIdentList, String> {
        // Identifiers
        let mut idents = Vec::new();
        // Loop
        loop {
            // Attempt to parse an identifier
            match self.force_typed_ident()? {
                Some(tid) => {
                    // Push found identifier string
                    idents.push(tid);
                    // Break if no comma
                    if self.expect(TokenType::Comma)?.is_none() { break }
                },
                _ => break
            };
        };
        // Return the list
        Ok(idents)
    }
    fn exprlist(&mut self) -> Result<ExprList, String> {
        // Expressions
        let mut exprs = Vec::new();
        // Loop
        loop {
            // Attempt to parse an expression
            match self.expression()? {
                Some(e) => {
                    // Push expression to vector
                    exprs.push(e);
                    // Break if no comma
                    if self.expect(TokenType::Comma)?.is_none() { break }
                },
                _ => break
            };
        };
        // Return the list
        Ok(exprs)
    }
    fn parse_type(&mut self) -> Result<Type, String> {
        // Check for identifier
        let first = match self.expect(TokenType::Identifier)? {
            Some((_, TokenValue::String(s), _)) => {
                match s.as_str() {
                    "i64" => Type::Int(6),
                    "i32" => Type::Int(5),
                    "i16" => Type::Int(4),
                    "i8" => Type::Int(3),
                    _ => return Err(self.expected_err("Type"))
                }
            },
            _ => return Err(self.expected_err("Type"))
        };
        // Return monotype
        Ok(first)
    }
}

const X: (i64,) = (0,);