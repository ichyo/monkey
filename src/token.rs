#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Token {
    Illegal,
    Eof,

    Ident(u32),
    Int(i64),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Equal,
    NotEqual,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
