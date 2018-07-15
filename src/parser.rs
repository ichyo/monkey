use ast::*;
use lexer::Lexer;
use std::fmt;
use std::result;
use token::Token;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Option<Token>,
}

type Result<T> = result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(String);

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl Precedence {
    fn get(token: &Token) -> Precedence {
        match token {
            Token::Equal => Precedence::Equals,
            Token::NotEqual => Precedence::Equals,
            Token::Lt => Precedence::LessGreater,
            Token::Gt => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
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

    pub fn parse_program(&mut self) -> Result<Program> {
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
            Some(_) => {
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
        let mut left = self.parse_prefix()?;
        while let Some(token) = self.token {
            let tok_prec = Precedence::get(&token);
            if precedence < tok_prec {
                left = self.parse_infix(left)?;
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression> {
        match self.token {
            Some(Token::Ident(_)) => self.prefix_identifier(),
            Some(Token::Int(_)) => self.prefix_integer(),
            Some(Token::True) | Some(Token::False) => self.prefix_boolean(),
            Some(Token::Bang) => self.parse_unary(),
            Some(Token::Minus) => self.parse_unary(),
            Some(Token::LeftParen) => self.parse_grouped(),
            Some(t) => Err(format!("unknown token for prefix parse: {:?}", t).into()),
            None => Err(format!("no token found for prefix parse").into()),
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression> {
        match self.token {
            Some(Token::Plus) => self.parse_bin(left),
            Some(Token::Minus) => self.parse_bin(left),
            Some(Token::Asterisk) => self.parse_bin(left),
            Some(Token::Slash) => self.parse_bin(left),
            Some(Token::Lt) => self.parse_bin(left),
            Some(Token::Gt) => self.parse_bin(left),
            Some(Token::Equal) => self.parse_bin(left),
            Some(Token::NotEqual) => self.parse_bin(left),
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

    fn parse_grouped(&mut self) -> Result<Expression> {
        self.expect(&Token::LeftParen);
        let res = self.parse_expression();
        self.expect(&Token::RightParen);
        res
    }

    fn parse_bin(&mut self, left: Expression) -> Result<Expression> {
        let op = match self.token {
            Some(Token::Plus) => BinOp::Add,
            Some(Token::Minus) => BinOp::Sub,
            Some(Token::Asterisk) => BinOp::Mul,
            Some(Token::Slash) => BinOp::Div,
            Some(Token::Lt) => BinOp::Lt,
            Some(Token::Gt) => BinOp::Gt,
            Some(Token::Equal) => BinOp::Eq,
            Some(Token::NotEqual) => BinOp::Ne,
            _ => unreachable!(),
        };
        let precedence = Precedence::get(&self.token.unwrap());

        self.bump();

        let right = self.parse_expression_inner(precedence)?;
        Ok(Expression {
            node: ExpressionKind::Bin(BinExpression {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
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

    fn prefix_boolean(&mut self) -> Result<Expression> {
        let boolean = self.parse_boolean()?;
        Ok(Expression {
            node: ExpressionKind::BooleanLiteral(boolean),
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

    fn parse_boolean(&mut self) -> Result<BooleanLiteral> {
        match self.token {
            Some(Token::True) => {
                self.bump();
                Ok(BooleanLiteral { value: true })
            }
            Some(Token::False) => {
                self.bump();
                Ok(BooleanLiteral { value: false })
            }
            Some(tok) => Err(format!("[integer] unexpected token: {:?}", tok).into()),
            None => Err(format!("[integer] no token found").into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
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
            if let StatementKind::Return(_) = &stmt.node {
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
            if let ExpressionKind::Identifier(_) = &stmt.expr.node {
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
                panic!("not integer");
            }
        } else {
            panic!("not expression statement");
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![("true;", true), ("false;", false)];

        for (input, value) in tests {
            let mut p = Parser::new(input);
            let program = p.parse_program().unwrap();
            assert_eq!(1, program.statements.len());

            if let StatementKind::Expression(stmt) = &program.statements[0].node {
                if let ExpressionKind::BooleanLiteral(b) = &stmt.expr.node {
                    assert_eq!(value, b.value);
                } else {
                    panic!("not boolean");
                }
            } else {
                panic!("not expression statement");
            }
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

    #[test]
    fn test_parsing_infix_expressions() {
        let tests = vec![
            ("1 + 2;", 1, BinOp::Add, 2),
            ("1 - 2;", 1, BinOp::Sub, 2),
            ("1 * 2;", 1, BinOp::Mul, 2),
            ("1 / 2;", 1, BinOp::Div, 2),
            ("1 == 2;", 1, BinOp::Eq, 2),
            ("1 != 2;", 1, BinOp::Ne, 2),
            ("1 < 2;", 1, BinOp::Lt, 2),
            ("1 > 2;", 1, BinOp::Gt, 2),
        ];
        for (input, left, op, right) in tests {
            let mut p = Parser::new(input);
            let program = p.parse_program().unwrap();

            assert_eq!(1, program.statements.len());

            if let StatementKind::Expression(e) = &program.statements[0].node {
                if let ExpressionKind::Bin(p) = &e.expr.node {
                    assert_eq!(op, p.op);
                    test_integer_literal(&p.left, left);
                    test_integer_literal(&p.right, right);
                } else {
                    panic!("wrong expression kind");
                }
            } else {
                panic!("wrong statement kind");
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-X0) * X1)"),
            ("!-a", "(!(-X0))"),
            ("a + b + c", "((X0 + X1) + X2)"),
            ("a + b - c", "((X0 + X1) - X2)"),
            ("a * b * c", "((X0 * X1) * X2)"),
            ("a * b / c", "((X0 * X1) / X2)"),
            ("a + b / c", "(X0 + (X1 / X2))"),
            (
                "a + b * c + d / e - f",
                "(((X0 + (X1 * X2)) + (X3 / X4)) - X5)",
            ),
            ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true != false == false", "((true != false) == false)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];
        for (input, expected) in tests {
            let mut p = Parser::new(input);
            let program = p.parse_program().unwrap();
            assert_eq!(expected, program.code());
        }
    }
}
