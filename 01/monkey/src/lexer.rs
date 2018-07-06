use std::iter::Peekable;
use std::str::Chars;
use token::Token;

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.chars().peekable(),
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = self.input.next()
    }

    fn next_token(&mut self) -> Token {
        let tok = match self.ch {
            Some('=') => Token::Assign,
            Some(';') => Token::Semicolon,
            Some('(') => Token::LeftParen,
            Some(')') => Token::RightParen,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::LeftBrace,
            Some('}') => Token::RightBrace,
            None => Token::Eof,
            _ => Token::Illegal,
        };
        self.read_char();
        tok
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
}
