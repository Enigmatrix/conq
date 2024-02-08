use crate::err::{EvalError, Type};
use crate::expr::Expr;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Void,
    Bool(bool),
    // Float(f64),
    Int(i64),
    // String(String),
    Fn { params: Vec<Ident>, expr: Box<Expr> },
}

impl Value {
    pub fn to_bool(&self) -> Result<bool, EvalError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(EvalError::TypeError {
                expected: Type::Bool,
                actual: self.clone(),
            }),
        }
    }

    pub fn to_int(&self) -> Result<i64, EvalError> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(EvalError::TypeError {
                expected: Type::Int,
                actual: self.clone(),
            }),
        }
    }
}
