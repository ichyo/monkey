use ast::*;
use object::Object;

pub fn eval(p: &Program) -> Object {
    eval_program(p)
}

fn eval_program(p: &Program) -> Object {
    let mut res = Object::Unit;
    for stmt in &p.statements {
        res = eval_statement(stmt);
        if let Object::Return(val) = res {
            return *val;
        }
    }
    res
}

fn eval_block_statements(b: &BlockStatement) -> Object {
    let mut res = Object::Unit;
    for stmt in &b.statements {
        res = eval_statement(stmt);
        if let Object::Return(_) = res {
            return res;
        }
    }
    res
}

fn eval_statement(stmt: &Statement) -> Object {
    match &stmt.node {
        StatementKind::Let(let_stmt) => unimplemented!(),
        StatementKind::Return(ret_stmt) => Object::Return(Box::new(eval_expr(&ret_stmt.value))),
        StatementKind::Expression(expr_stmt) => eval_expr(&expr_stmt.expr),
    }
}

fn eval_expr(expr: &Expression) -> Object {
    match &expr.node {
        ExpressionKind::Identifier(x) => unimplemented!(),
        ExpressionKind::IntegerLiteral(x) => Object::Integer(x.value),
        ExpressionKind::BooleanLiteral(x) => Object::Boolean(x.value),
        ExpressionKind::Unary(x) => eval_unary(x),
        ExpressionKind::Bin(x) => eval_bin(x),
        ExpressionKind::If(x) => eval_if(x),
        ExpressionKind::Func(x) => unimplemented!(),
        ExpressionKind::Call(x) => unimplemented!(),
    }
}

fn eval_if(if_expr: &IfExpression) -> Object {
    let cond = eval_expr(&if_expr.cond);
    match cond {
        Object::Boolean(cond) => {
            if cond {
                eval_block_statements(&if_expr.cons)
            } else if let Some(alt) = &if_expr.alt {
                eval_block_statements(alt)
            } else {
                Object::Unit
            }
        }
        _ => unimplemented!(),
    }
}

fn eval_unary(unary: &UnaryExpression) -> Object {
    let right = eval_expr(&unary.expr);
    match unary.op {
        UnOp::Not => match right {
            Object::Boolean(b) => Object::Boolean(!b),
            _ => unimplemented!(),
        },
        UnOp::Neg => match right {
            Object::Integer(x) => Object::Integer(-x),
            _ => unimplemented!(),
        },
    }
}

fn eval_bin(bin: &BinExpression) -> Object {
    let left = eval_expr(&bin.left);
    let right = eval_expr(&bin.right);
    match bin.op {
        BinOp::Add => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
            _ => unimplemented!(),
        },
        BinOp::Sub => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left - right),
            _ => unimplemented!(),
        },
        BinOp::Mul => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left * right),
            _ => unimplemented!(),
        },
        BinOp::Div => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left / right),
            _ => unimplemented!(),
        },
        BinOp::Lt => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left < right),
            _ => unimplemented!(),
        },
        BinOp::Gt => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left > right),
            _ => unimplemented!(),
        },
        BinOp::Eq => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left == right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left == right),
            _ => unimplemented!(),
        },
        BinOp::Ne => match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left != right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left != right),
            _ => unimplemented!(),
        },
    }
}

#[cfg(test)]
mod test {
    use eval::eval;
    use object::Object;
    use parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-3", -3),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("!true", false),
            ("!false", true),
            ("!!true", true),
            ("!!false", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Unit),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Unit),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2*5; 9;", Object::Integer(10)),
            ("9; return 2*5; 9;", Object::Integer(10)),
            (
                "if (true) { if (true) { return 2*5; } return 1; }; 9;",
                Object::Integer(10),
            ),
            (
                "if (true) { if (false) { return 2*5; } return 1; }; 9;",
                Object::Integer(1),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            assert_eq!(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let mut p = Parser::new(input);
        let program = p.parse_program().expect("Parse failed");
        eval(&program)
    }

    fn test_integer_object(object: &Object, expected: i64) {
        match *object {
            Object::Integer(value) => {
                assert_eq!(value, expected);
            }
            _ => {
                panic!(format!("Object is not Integer. got={:?}", object));
            }
        }
    }

    fn test_boolean_object(object: &Object, expected: bool) {
        match *object {
            Object::Boolean(value) => {
                assert_eq!(value, expected);
            }
            _ => {
                panic!(format!("Object is not Boolean. got={:?}", object));
            }
        }
    }
}
