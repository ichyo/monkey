#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
}
