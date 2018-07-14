use std::collections::HashMap;
use std::str::Chars;
use token::Token;

pub struct Lexer<'a> {
    input: Chars<'a>,
    ch: Option<char>,
    eof: bool,
    dict: HashMap<String, u32>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.chars(),
            ch: None,
            eof: false,
            dict: HashMap::new(),
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = self.input.next()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.eof {
            return None;
        }
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
                self.lookup_ident(identifier)
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
        if tok == Token::Eof {
            self.eof = true;
        }
        Some(tok)
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

    fn read_number(&mut self) -> i64 {
        let mut res = 0i64;
        while let Some(c) = self.ch {
            if !c.is_digit(10) {
                break;
            }
            res = res * 10 + (c as u8 - b'0') as i64;
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

    fn lookup_ident(&mut self, ident: String) -> Token {
        match ident.as_ref() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            _ => Token::Ident(self.loopup_dict(ident)),
        }
    }

    fn loopup_dict(&mut self, ident: String) -> u32 {
        if self.dict.contains_key(&ident) {
            *self.dict.get(&ident).unwrap()
        } else {
            let res = self.dict.len() as u32;
            self.dict.insert(ident, res);
            res
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_token()
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
        let actual: Vec<_> = l.collect();

        assert_eq!(tests, actual);
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
            Token::Ident(0), // five
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(1), // ten
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(2), // add
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Ident(3), // x
            Token::Comma,
            Token::Ident(4), // y
            Token::RightParen,
            Token::LeftBrace,
            Token::Ident(3), // x
            Token::Plus,
            Token::Ident(4), // y
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(5), // result
            Token::Assign,
            Token::Ident(2), // add
            Token::LeftParen,
            Token::Ident(0), // five
            Token::Comma,
            Token::Ident(1), // ten
            Token::RightParen,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);
        let mut actual: Vec<_> = l.collect();
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
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
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
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);
        let mut actual: Vec<_> = l.collect();
        assert_eq!(tests, actual);
    }
}
