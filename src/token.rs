#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum Token<'a> {
    Illegal,
    Eof,

    Ident(&'a str),
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
