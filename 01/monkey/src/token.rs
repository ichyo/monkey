#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident,
    Int,
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
