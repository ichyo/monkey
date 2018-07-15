pub trait Code {
    fn code(&self) -> String;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Code for Program {
    fn code(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.code())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Statement {
    pub node: StatementKind,
}

impl Code for Statement {
    fn code(&self) -> String {
        match &self.node {
            StatementKind::Let(stmt) => stmt.code(),
            StatementKind::Return(stmt) => stmt.code(),
            StatementKind::Expression(stmt) => stmt.code(),
        }
    }
}

#[derive(Debug)]
pub enum StatementKind {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Box<Identifier>,
    pub value: Box<Expression>,
}

impl Code for LetStatement {
    fn code(&self) -> String {
        format!("let {} = {};", self.name.code(), self.value.code())
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Box<Expression>,
}

impl Code for ReturnStatement {
    fn code(&self) -> String {
        format!("return {};", self.value.code())
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: Box<Expression>,
}

impl Code for ExpressionStatement {
    fn code(&self) -> String {
        format!("{}", self.expr.code())
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub value: u32,
}

impl Code for Identifier {
    fn code(&self) -> String {
        format!("X{}", self.value)
    }
}

#[derive(Debug)]
pub struct Expression {
    pub node: ExpressionKind,
}

impl Code for Expression {
    fn code(&self) -> String {
        match &self.node {
            ExpressionKind::Identifier(x) => x.code(),
            ExpressionKind::IntegerLiteral(x) => x.code(),
            ExpressionKind::Unary(x) => x.code(),
            ExpressionKind::Bin(x) => x.code(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Unary(UnaryExpression),
    Bin(BinExpression),
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl Code for IntegerLiteral {
    fn code(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Not,
    Neg,
}

impl UnOp {
    pub fn to_string(op: UnOp) -> &'static str {
        match op {
            UnOp::Not => "!",
            UnOp::Neg => "-",
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub op: UnOp,
    pub expr: Box<Expression>,
}

impl Code for UnaryExpression {
    fn code(&self) -> String {
        format!("({}{})", UnOp::to_string(self.op), self.expr.code())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Lt,  // <
    Gt,  // >
    Ne,  // !=
    Eq,  // ==
}

impl BinOp {
    pub fn to_string(op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Ne => "!=",
            BinOp::Eq => "==",
        }
    }
}

#[derive(Debug)]
pub struct BinExpression {
    pub op: BinOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Code for BinExpression {
    fn code(&self) -> String {
        format!(
            "({} {} {})",
            self.left.code(),
            BinOp::to_string(self.op),
            self.right.code()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_code() {
        let program = Program {
            statements: vec![
                Statement {
                    node: StatementKind::Let(LetStatement {
                        name: Box::new(Identifier { value: 1 }),
                        value: Box::new(Expression {
                            node: ExpressionKind::Identifier(Identifier { value: 2 }),
                        }),
                    }),
                },
                Statement {
                    node: StatementKind::Return(ReturnStatement {
                        value: Box::new(Expression {
                            node: ExpressionKind::Identifier(Identifier { value: 1 }),
                        }),
                    }),
                },
            ],
        };
        assert_eq!("let X1 = X2;\nreturn X1;", program.code());
    }
}
