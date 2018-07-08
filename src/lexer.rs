use std::str::Chars;
use token::Token;

pub struct Lexer<'a> {
    input: Chars<'a>,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.chars(),
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = self.input.next()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            None => Token::Eof,
            Some('=') => {
                self.read_char();
                match self.ch {
                    Some('=') => {
                        self.read_char();
                        Token::Equal
                    }
                    _ => Token::Assign,
                }
            }
            Some(';') => {
                self.read_char();
                Token::Semicolon
            }
            Some('(') => {
                self.read_char();
                Token::LeftParen
            }
            Some(')') => {
                self.read_char();
                Token::RightParen
            }
            Some(',') => {
                self.read_char();
                Token::Comma
            }
            Some('+') => {
                self.read_char();
                Token::Plus
            }
            Some('-') => {
                self.read_char();
                Token::Minus
            }
            Some('!') => {
                self.read_char();
                match self.ch {
                    Some('=') => {
                        self.read_char();
                        Token::NotEqual
                    }
                    _ => Token::Bang,
                }
            }
            Some('/') => {
                self.read_char();
                Token::Slash
            }
            Some('*') => {
                self.read_char();
                Token::Asterisk
            }
            Some('<') => {
                self.read_char();
                Token::Lt
            }
            Some('>') => {
                self.read_char();
                Token::Gt
            }
            Some('{') => {
                self.read_char();
                Token::LeftBrace
            }
            Some('}') => {
                self.read_char();
                Token::RightBrace
            }
            Some(c) if is_letter(c) => {
                let identifier = self.read_identifier();
                lookup_ident(identifier)
            }
            Some(c) if c.is_digit(10) => {
                let number = self.read_number();
                Token::Int(number)
            }
            Some(_) => {
                self.read_char();
                Token::Illegal
            }
        };
        tok
    }

    fn read_identifier(&mut self) -> String {
        let mut res = String::new();
        while let Some(c) = self.ch {
            if !is_letter(c) {
                break;
            }
            res.push(c);
            self.read_char();
        }
        res
    }

    fn read_number(&mut self) -> String {
        let mut res = String::new();
        while let Some(c) = self.ch {
            if !c.is_digit(10) {
                break;
            }
            res.push(c);
            self.read_char();
        }
        res
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if !c.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }
}

fn is_letter(c: char) -> bool {
    match c {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        _ => false,
    }
}

fn lookup_ident(ident: String) -> Token {
    match ident.as_ref() {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        _ => Token::Ident(ident),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = vec![
            Token::Assign,
            Token::Plus,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);

        for t in tests {
            assert_eq!(t, l.next_token());
        }
    }

    #[test]
    fn test_next_token2() {
        let input = r"
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
";
        let tests = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LeftParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);

        let mut actual = Vec::new();
        loop {
            let t = l.next_token();
            let end = t == Token::Illegal || t == Token::Eof;
            actual.push(t);
            if end {
                break;
            }
        }
        assert_eq!(tests, actual);
    }

    #[test]
    fn test_next_token3() {
        let input = r"
        !-/*5;
        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        ";

        let tests = vec![
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RightBrace,
            Token::Int("10".to_string()),
            Token::Equal,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEqual,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);

        let mut actual = Vec::new();
        loop {
            let t = l.next_token();
            let end = t == Token::Illegal || t == Token::Eof;
            actual.push(t);
            if end {
                break;
            }
        }
        assert_eq!(tests, actual);
    }
}