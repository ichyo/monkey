pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

pub struct Statement {
    pub node: StatementKind,
}

pub enum StatementKind {
    Let(Box<LetStatement>),
    Return,
}

pub struct LetStatement {
    pub name: Box<Identifier>,
    pub value: Box<Expression>,
}

pub struct Identifier {
    pub value: u32,
}

pub struct Expression {
    pub node: ExpressionKind,
}

pub enum ExpressionKind {
    Identifier(Box<Identifier>),
}
