pub trait Code {
    fn code(&self) -> String;
}

#[derive(Debug)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Code for Program<'a> {
    fn code(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.code())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl<'a> Program<'a> {
    pub fn new() -> Program<'a> {
        Program {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Statement<'a> {
    pub node: StatementKind<'a>,
}

impl<'a> Code for Statement<'a> {
    fn code(&self) -> String {
        match &self.node {
            StatementKind::Let(stmt) => stmt.code(),
            StatementKind::Return(stmt) => stmt.code(),
            StatementKind::Expression(stmt) => stmt.code(),
        }
    }
}

#[derive(Debug)]
pub enum StatementKind<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>),
}

#[derive(Debug)]
pub struct LetStatement<'a> {
    pub name: Box<Identifier<'a>>,
    pub value: Box<Expression<'a>>,
}

impl<'a> Code for LetStatement<'a> {
    fn code(&self) -> String {
        format!("let {} = {};", self.name.code(), self.value.code())
    }
}

#[derive(Debug)]
pub struct ReturnStatement<'a> {
    pub value: Box<Expression<'a>>,
}

impl<'a> Code for ReturnStatement<'a> {
    fn code(&self) -> String {
        format!("return {};", self.value.code())
    }
}

#[derive(Debug)]
pub struct ExpressionStatement<'a> {
    pub expr: Box<Expression<'a>>,
}

impl<'a> Code for ExpressionStatement<'a> {
    fn code(&self) -> String {
        format!("{}", self.expr.code())
    }
}

#[derive(Debug)]
pub struct BlockStatement<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Code for BlockStatement<'a> {
    fn code(&self) -> String {
        format!(
            "{{ {} }}",
            self.statements
                .iter()
                .map(|s| s.code())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug)]
pub struct Identifier<'a> {
    pub value: &'a str,
}

impl<'a> Code for Identifier<'a> {
    fn code(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Expression<'a> {
    pub node: ExpressionKind<'a>,
}

impl<'a> Code for Expression<'a> {
    fn code(&self) -> String {
        match &self.node {
            ExpressionKind::Identifier(x) => x.code(),
            ExpressionKind::IntegerLiteral(x) => x.code(),
            ExpressionKind::BooleanLiteral(x) => x.code(),
            ExpressionKind::Unary(x) => x.code(),
            ExpressionKind::Bin(x) => x.code(),
            ExpressionKind::If(x) => x.code(),
            ExpressionKind::Func(x) => x.code(),
            ExpressionKind::Call(x) => x.code(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind<'a> {
    Identifier(Identifier<'a>),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    Unary(UnaryExpression<'a>),
    Bin(BinExpression<'a>),
    If(IfExpression<'a>),
    Func(FunctionalLiteral<'a>),
    Call(CallExpression<'a>),
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

#[derive(Debug)]
pub struct BooleanLiteral {
    pub value: bool,
}

impl Code for BooleanLiteral {
    fn code(&self) -> String {
        format!("{}", if self.value { "true" } else { "false" })
    }
}

#[derive(Debug)]
pub struct FunctionalLiteral<'a> {
    pub params: Vec<Identifier<'a>>,
    pub body: Box<BlockStatement<'a>>,
}

impl<'a> Code for FunctionalLiteral<'a> {
    fn code(&self) -> String {
        format!(
            "fn ({}) {}",
            self.params
                .iter()
                .map(|x| x.code())
                .collect::<Vec<_>>()
                .join(", "),
            self.body.code()
        )
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
pub struct UnaryExpression<'a> {
    pub op: UnOp,
    pub expr: Box<Expression<'a>>,
}

impl<'a> Code for UnaryExpression<'a> {
    fn code(&self) -> String {
        format!("({}{})", UnOp::to_string(self.op), self.expr.code())
    }
}

#[derive(Debug)]
pub struct IfExpression<'a> {
    pub cond: Box<Expression<'a>>,
    pub cons: Box<BlockStatement<'a>>,
    pub alt: Option<Box<BlockStatement<'a>>>,
}

impl<'a> Code for IfExpression<'a> {
    fn code(&self) -> String {
        match &self.alt {
            Some(alt) => format!(
                "if {} {} else {}",
                self.cond.code(),
                self.cons.code(),
                alt.code()
            ),
            None => format!("if {} {}", self.cond.code(), self.cons.code()),
        }
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
pub struct BinExpression<'a> {
    pub op: BinOp,
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

impl<'a> Code for BinExpression<'a> {
    fn code(&self) -> String {
        format!(
            "({} {} {})",
            self.left.code(),
            BinOp::to_string(self.op),
            self.right.code()
        )
    }
}

#[derive(Debug)]
pub struct CallExpression<'a> {
    pub func: Box<Expression<'a>>,
    pub args: Vec<Expression<'a>>,
}

impl<'a> Code for CallExpression<'a> {
    fn code(&self) -> String {
        format!(
            "{}({})",
            self.func.code(),
            self.args
                .iter()
                .map(|x| x.code())
                .collect::<Vec<_>>()
                .join(", "),
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
                        name: Box::new(Identifier { value: "x" }),
                        value: Box::new(Expression {
                            node: ExpressionKind::Identifier(Identifier { value: "y" }),
                        }),
                    }),
                },
                Statement {
                    node: StatementKind::Return(ReturnStatement {
                        value: Box::new(Expression {
                            node: ExpressionKind::Identifier(Identifier { value: "x" }),
                        }),
                    }),
                },
            ],
        };
        assert_eq!("let x = y;\nreturn x;", program.code());
    }
}
