trait Code {
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
    pub name: Identifier,
    pub value: Expression,
}

impl Code for LetStatement {
    fn code(&self) -> String {
        format!("let {} = {};", self.name.code(), self.value.code())
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl Code for ReturnStatement {
    fn code(&self) -> String {
        format!("return {};", self.value.code())
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

impl Code for ExpressionStatement {
    fn code(&self) -> String {
        format!("{};", self.expr.code())
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
            ExpressionKind::Integer => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    Identifier(Identifier),
    Integer,
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_code() {
        let program = Program {
            statements: vec![
                Statement {
                    node: StatementKind::Let(LetStatement {
                        name: Identifier { value: 1 },
                        value: Expression {
                            node: ExpressionKind::Identifier(Identifier { value: 2 }),
                        },
                    }),
                },
                Statement {
                    node: StatementKind::Return(ReturnStatement {
                        value: Expression {
                            node: ExpressionKind::Identifier(Identifier { value: 1 }),
                        },
                    }),
                },
            ],
        };
        assert_eq!("let X1 = X2;\nreturn X1", program.code());
    }
}
