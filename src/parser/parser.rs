use crate::lexer::lexer::*;
use std::collections::HashMap;

pub type Program = (HashMap<Ident, Function>, SymbolTable);
pub type Function = (IdentList, Block, Location);
pub type SymbolTable = Vec<(usize, HashMap<String, (usize, usize, bool, (Option<usize>, Option<usize>), )>)>; // Pushed with, fixed memory location, has been declared in this scope, location(s) stored at
pub type Block = (Vec<Statement>, usize);

pub enum StatementBody {
    ExprStatement(Expression),
    ReturnStatement(Expression),
    LetStmt(Ident, Expression),
    AssignStmt(Ident, Expression),
    IfStmt(Expression, Block, Option<Block>),
    WhileStmt(Expression, Block),
    BlockStmt(Block),
    BreakStmt,
    EmitString(String),
    Emit(Expression)
}

pub type Statement = (StatementBody, Location);

#[derive(Clone)]
pub enum ExpressionBody {
    UopExpression(Uop, Box<Expression>),
    BopExpression(Bop, Box<Expression>, Box<Expression>),
    IntLiteral(bool, usize),
    VariableExpression(Ident),
    CallExpression(Ident, ExprList)
}

pub type Expression = (ExpressionBody, Location);

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

pub type IdentList = Vec<Ident>;
pub type StmtList = Vec<Statement>;
pub type ExprList = Vec<Expression>;

pub type Location = (usize, usize);

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
        // Create symbol table with one entry (global scope)
        let mut table: SymbolTable = vec![ (0, HashMap::new()) ];
        // Capture functions until can't anymore
        loop {
            // Check if function
            match self.function(&mut table)? {
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
        Ok((fns, table))
    }
    fn block(&mut self, table: &mut SymbolTable, parent: usize) -> Result<Option<Block>, String> {
        // Check for open bracket
        match self.expect(TokenType::LBracket)? {
            None => return Ok(None),
            _ => ()
        };
        // Create new node in the symbol table (point back to parent)
        table.push((parent, HashMap::new()));
        // Get new scope id
        let scope_id = table.len() - 1;
        // Parse statement list
        let stmts = self.stmtlist(table, scope_id)?;
        // Check for close bracket
        self.expect_err(TokenType::RBracket)?;
        // Return and attach current block's scope id
        Ok(Some((stmts, scope_id)))
    }
    fn function(&mut self, table: &mut SymbolTable) -> Result<Option<(String, Function)>, String> {
        // Check for function keyword
        let loc = match self.expect(TokenType::FunKw)? {
            Some((_, _, loc)) => loc.clone(),
            None => return Ok(None)
        };
        // Extract ident
        let id = match self.expect_err(TokenType::Identifier)? {
            (_, TokenValue::String(s), _) => s.clone(),
            _ => return Err(self.expected_err("identifier"))
        };
        // Expect open paren
        let idlist = 
            if self.expect(TokenType::LParen)?.is_some() {
                // Parse identifier list
                let idlist = self.identlist()?;
                // Expect closing paren
                self.expect_err(TokenType::RParen)?;
                // Return ilist
                idlist
            } else {
                // Empty list
                Vec::new()
            };
        // Create symbol table entry just for function parameters (this allows for redefining in body)
        let mut vars = HashMap::new();
        for id in &idlist {
            match vars.insert(id.clone(), (0, 0, true, (None, None))) {
                None => (),
                Some(_) => return Err(self.generic_err("Duplicate function parameters"))
            }
        };
        table.push((0, vars));
        // Expect block (parent is function parameter scope)
        let b = match self.block(table, table.len() - 1)? { Some(b) => b, _ => return Err(self.expected_err("Block")) };
        // Return
        Ok(Some((id, (idlist, b, loc))))
    }
    fn statement(&mut self, table: &mut SymbolTable, parent: usize) -> Result<Option<Statement>, String> {
        // Mark position
        let pos = self.mark();
        // Skip to next token for lookahead and save location of statement
        let loc = match self.tokenizer.get_token()? {
            (_, _, loc) => loc.clone()
        };
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
            return Ok(Some((StatementBody::AssignStmt(id, ex), loc)))
        };
        // Reset position
        self.reset(pos);
        // Check for let token
        if self.expect(TokenType::LetKw)?.is_some() {
            // Parse a typed identifier
            let tid = match self.expect_err(TokenType::Identifier)? {
                (_, TokenValue::String(x), _) => x.clone(),
                _ => panic!()
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
            // Create a new symbol table entry for parent scope
            match table[parent].1.insert(tid.clone(), (0, 0, false, (None, None))) {
                Some(_) => return Err(self.generic_err("Duplicate variables in scope")),
                _ => ()
            };
            // Put together and return
            return Ok(Some((StatementBody::LetStmt(tid, e), loc)))
        };
        // Check for return keyword
        if self.expect(TokenType::ReturnKw)?.is_some() {
            // Parse an expression
            match self.expression()? {
                Some(e) => {
                    // Expect a semicolon
                    self.expect_err(TokenType::Semi)?;
                    // Return
                    return Ok(Some((StatementBody::ReturnStatement(e), loc)))
                },
                None => return Err(self.expected_err("Expression"))
            }
        };
        // Check for if keyword
        if self.expect(TokenType::IfKw)?.is_some() {
            // Parse an expression
            let cond = match self.expression()? {
                Some(e) => e,
                None => return Err(self.expected_err("Expression"))
            };
            // Expect block
            let ifbody = match self.block(table, parent)? { Some(b) => b, _ => return Err(self.expected_err("Block")) };
            // Check if else keyword
            let elsebody = if self.expect(TokenType::ElseKw)?.is_some() {
                // Parse else body
                let b = match self.block(table, parent)? { Some(b) => b, _ => return Err(self.expected_err("Block")) };
                Some(b)
            } else {
                // Return empty body
                None
            };
            // Put together if statement
            return Ok(Some((StatementBody::IfStmt(cond, ifbody, elsebody), loc)))
        }
        // Check for while keyword
        if self.expect(TokenType::WhileKw)?.is_some() {
            // Parse an expression
            let cond = match self.expression()? {
                Some(e) => e,
                None => return Err(self.expected_err("Expression"))
            };
            // Expect block
            let whilebody = match self.block(table, parent)? { Some(b) => b, _ => return Err(self.expected_err("Block")) };
            // Put together while statement
            return Ok(Some((StatementBody::WhileStmt(cond, whilebody), loc)))
        }
        // Check for break keyword
        if self.expect(TokenType::BreakKw)?.is_some() {
            // Expect semicolon
            self.expect_err(TokenType::Semi)?;
            // Return break statement
            return Ok(Some((StatementBody::BreakStmt, loc)))
        }
        // Check for emit keyword
        if self.expect(TokenType::EmitKw)?.is_some() {
            // Check for string literal
            match self.expect(TokenType::StringLiteral)? {
                Some((_, TokenValue::String(literal), _)) => {
                    // Return emit statement
                    return Ok(Some((StatementBody::EmitString(literal.clone()), loc)))
                },
                None => {
                    // Parse expression
                    let e1 = match self.expression()? {
                        Some(e) => e,
                        None => return Err(self.expected_err("Expression"))
                    };
                    // Expect semicolon
                    self.expect_err(TokenType::Semi)?;
                    // Return emit statement
                    return Ok(Some((StatementBody::Emit(e1), loc)))
                },
                _ => panic!()
            }
        }
        // Attempt to parse block
        match self.block(table, parent)? {
            Some(b) => return Ok(Some((StatementBody::BlockStmt(b), loc))),
            None => {
                // Reset position
                self.reset(pos);
            }
        };
        // Attempt to parse expression
        match self.expression()? {
            Some(e) => {
                // Expect a semicolon
                self.expect_err(TokenType::Semi)?;
                // Return
                Ok(Some((StatementBody::ExprStatement(e), loc)))
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
            |acc, bop| (ExpressionBody::BopExpression(bop.0, Box::new(acc.clone()), Box::new(bop.1)), acc.1)
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
        match self.expect(TokenType::Minus)?.cloned() {
            Some((_, _, loc)) => {
                // Parse expression
                return match self.e2()? {
                    Some(e) => {
                        // Check type of expression
                        match e {
                            // Int literal, flip sign
                            (ExpressionBody::IntLiteral(sign, magnitude), _) => Ok(Some((ExpressionBody::IntLiteral(!sign, magnitude), loc))),
                            // Anything else, create uop expression
                            _ => Ok(Some((ExpressionBody::UopExpression(Uop::NegUop, Box::new(e)), loc)))
                        }
                    },
                    None => Err(self.expected_err("Expression"))
                };
            },
            None => ()
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
            let (vstr, loc) = match v {
                (ExpressionBody::VariableExpression(s), loc) => (s, loc),
                _ => return Err(self.generic_err("Cannot call a non-function"))
            };
            // Parse expression list
            let elist = self.exprlist()?;
            // Expect right paren
            self.expect_err(TokenType::RParen)?;
            // Return
            return Ok(Some((ExpressionBody::CallExpression(vstr, elist), loc)))
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
        // Get current location for single-token values
        let loc = self.tokenizer.peek_token()?.2.clone();
        // Check for ident
        match self.expect(TokenType::Identifier)? {
            Some((_, TokenValue::String(x), _)) => return Ok(Some((ExpressionBody::VariableExpression(x.clone()), loc))),
            _ => ()
        };
        // Check for integer literal
        match self.expect(TokenType::Integer)? {
            Some((_, TokenValue::Integer(x), _)) => return Ok(Some((ExpressionBody::IntLiteral(false, x.clone()), loc))),
            _ => ()
        };
        // No value matched
        Ok(None)
    }
    fn stmtlist(&mut self, table: &mut SymbolTable, parent: usize) -> Result<StmtList, String> {
        // Statements
        let mut stmts = Vec::new();
        // Loop
        loop {
            // Attempt to parse a statement
            match self.statement(table, parent)? {
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
                Some((_, TokenValue::String(tid), _)) => {
                    // Push found identifier string
                    idents.push(tid.clone());
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
}