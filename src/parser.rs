use ast::*;
use lexer::Lexer;
use std::collections::HashMap;
use std::fmt;
use std::result;
use token::Token;

struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Option<Token>,
}

type Result<T> = result::Result<T, ParseError>;

#[derive(Debug)]
struct ParseError(String);

impl From<String> for ParseError {
    fn from(w: String) -> ParseError {
        ParseError(w)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse: {}", self.0)
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        let mut p = Parser {
            lexer: Lexer::new(input),
            token: None,
        };
        p.bump();
        p
    }

    fn bump(&mut self) {
        self.token = self.lexer.next();
    }

    fn eat(&mut self, token: &Token) -> bool {
        match self.token {
            Some(tok) if tok == *token => {
                self.bump();
                true
            }
            _ => false,
        }
    }

    fn expect(&mut self, token: &Token) -> Result<()> {
        if !self.eat(token) {
            Err(format!("expected {:?} but actually got {:?}", token, self.token).into())
        } else {
            Ok(())
        }
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::new();
        loop {
            match self.token {
                Some(Token::Eof) => break,
                None => break,
                _ => {
                    let statement = self.parse_statement()?;
                    program.statements.push(statement);
                }
            }
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let node = match self.token {
            Some(Token::Let) => {
                let s = self.parse_let_statement()?;
                Ok(StatementKind::Let(s))
            }
            Some(Token::Return) => {
                let s = self.parse_return_statement()?;
                Ok(StatementKind::Return(s))
            }
            Some(tok) => {
                let s = self.parse_expression_statement()?;
                Ok(StatementKind::Expression(s))
            }
            None => Err(format!("[stmt] no token found")),
        }?;
        Ok(Statement { node })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        // TODO: implement
        self.bump();
        Ok(Expression {
            node: ExpressionKind::Identifier(Identifier { value: 0 }),
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        self.expect(&Token::Let)?;
        let ident = self.parse_identifier()?;
        self.expect(&Token::Assign)?;
        let expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(LetStatement {
            name: ident,
            value: expr,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        self.expect(&Token::Return)?;
        let expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(ReturnStatement { value: expr })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(ExpressionStatement { expr })
    }

    fn parse_identifier(&mut self) -> Result<Identifier> {
        match self.token {
            Some(Token::Ident(value)) => {
                self.bump();
                Ok(Identifier { value })
            }
            Some(tok) => Err(format!("[ident] unexpected token: {:?}", tok).into()),
            None => Err(format!("[ident] no token found").into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{ExpressionKind, StatementKind};
    use parser::Parser;

    #[test]
    fn test_let_statement() {
        let input = r"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        let x = 5;
        ";

        let mut p = Parser::new(input);
        let program = p.parse_program().unwrap();

        let tests = [0, 1, 2, 0];

        assert_eq!(tests.len(), program.statements.len());
        for (&t, ref stmt) in tests.iter().zip(program.statements.iter()) {
            if let StatementKind::Let(ref stmt) = stmt.node {
                assert_eq!(t, stmt.name.value);
            } else {
                panic!("it's not identifer");
            }
        }
    }

    #[test]
    fn test_return_statement() {
        let input = r"
        return 5;
        return 10;
        return 993322;
        ";
        let mut p = Parser::new(input);
        let program = p.parse_program().unwrap();
        assert_eq!(3, program.statements.len());

        for stmt in program.statements.iter() {
            if let StatementKind::Return(stmt) = &stmt.node {
            } else {
                panic!("it's not return");
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut p = Parser::new(input);
        let program = p.parse_program().unwrap();
        assert_eq!(1, program.statements.len());

        if let StatementKind::Expression(stmt) = &program.statements[0].node {
            if let ExpressionKind::Identifier(ident) = &stmt.expr.node {
            } else {
                panic!("not identifier");
            }
        } else {
            panic!("not expression statement");
        }
    }
}
