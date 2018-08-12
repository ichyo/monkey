#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Unit,
    Return(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(x) => x.to_string(),
            Object::Boolean(x) => x.to_string(),
            Object::Unit => "()".to_string(),
            Object::Return(x) => format!("Return({})", x.inspect()),
        }
    }
}
