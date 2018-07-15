use ast::*;
use lexer::Lexer;
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

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

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
        self.parse_expression_inner(Precedence::Lowest)
    }

    fn parse_expression_inner(&mut self, precedence: Precedence) -> Result<Expression> {
        self.prefix_parse()
    }

    fn prefix_parse(&mut self) -> Result<Expression> {
        match self.token {
            Some(Token::Ident(_)) => self.prefix_identifier(),
            Some(Token::Int(_)) => self.prefix_integer(),
            Some(Token::Bang) => self.parse_unary(),
            Some(Token::Minus) => self.parse_unary(),
            Some(t) => Err(format!("unknown token for prefix parse: {:?}", t).into()),
            None => Err(format!("no token found for prefix parse").into()),
        }
    }

    fn parse_unary(&mut self) -> Result<Expression> {
        let op = match self.token {
            Some(Token::Bang) => UnOp::Not,
            Some(Token::Minus) => UnOp::Neg,
            _ => unreachable!(),
        };
        self.bump();
        let expr = Box::new(self.parse_expression_inner(Precedence::Prefix)?);
        Ok(Expression {
            node: ExpressionKind::Unary(UnaryExpression { op, expr }),
        })
    }

    fn prefix_identifier(&mut self) -> Result<Expression> {
        let ident = self.parse_identifier()?;
        Ok(Expression {
            node: ExpressionKind::Identifier(ident),
        })
    }

    fn prefix_integer(&mut self) -> Result<Expression> {
        let integer = self.parse_integer()?;
        Ok(Expression {
            node: ExpressionKind::IntegerLiteral(integer),
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        self.expect(&Token::Let)?;
        let ident = self.parse_identifier()?;
        self.expect(&Token::Assign)?;
        let expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(LetStatement {
            name: Box::new(ident),
            value: Box::new(expr),
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        self.expect(&Token::Return)?;
        let expr = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        Ok(ReturnStatement {
            value: Box::new(expr),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let expr = self.parse_expression()?;
        self.eat(&Token::Semicolon);
        Ok(ExpressionStatement {
            expr: Box::new(expr),
        })
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

    fn parse_integer(&mut self) -> Result<IntegerLiteral> {
        match self.token {
            Some(Token::Int(value)) => {
                self.bump();
                Ok(IntegerLiteral { value })
            }
            Some(tok) => Err(format!("[integer] unexpected token: {:?}", tok).into()),
            None => Err(format!("[integer] no token found").into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{Expression, ExpressionKind, StatementKind, UnOp};
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

    #[test]
    fn test_integer_expression() {
        let input = "300;";

        let mut p = Parser::new(input);
        let program = p.parse_program().unwrap();
        assert_eq!(1, program.statements.len());

        if let StatementKind::Expression(stmt) = &program.statements[0].node {
            if let ExpressionKind::IntegerLiteral(integer) = &stmt.expr.node {
                assert_eq!(300, integer.value);
            } else {
                panic!("not identifier");
            }
        } else {
            panic!("not expression statement");
        }
    }

    #[test]
    fn test_parsing_unary_expressions() {
        let prefix_tests = vec![("!5", UnOp::Not, 5), ("-15", UnOp::Neg, 15)];

        for (input, operator, integer_value) in prefix_tests {
            let mut p = Parser::new(input);
            let program = p.parse_program().unwrap();
            assert_eq!(1, program.statements.len());

            if let StatementKind::Expression(e) = &program.statements[0].node {
                if let ExpressionKind::Unary(p) = &e.expr.node {
                    assert_eq!(operator, p.op);
                    test_integer_literal(&p.expr, integer_value);
                } else {
                    panic!("wrong expression kind");
                }
            } else {
                panic!("wrong statement kind");
            }
        }
    }

    fn test_integer_literal(e: &Expression, value: i64) {
        if let ExpressionKind::IntegerLiteral(integer) = &e.node {
            assert_eq!(value, integer.value);
        } else {
            panic!("wrong expression kind");
        }
    }
}
