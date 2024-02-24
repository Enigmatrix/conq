use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::sync::Arc;

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
    PredefinedFn { r#impl: PredefinedFnImpl },
}

#[derive(Clone)]
pub struct PredefinedFnImpl(Arc<dyn Fn(Vec<Value>) -> Result<Value, EvalError>>);

impl PredefinedFnImpl {
    pub fn new(inner: Arc<dyn Fn(Vec<Value>) -> Result<Value, EvalError>>) -> Self {
        PredefinedFnImpl(inner)
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, EvalError> {
        self.0(args)
    }
}

impl Debug for PredefinedFnImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.write_str("[predifined impl]")
    }
}

impl PartialEq for PredefinedFnImpl {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

impl Eq for PredefinedFnImpl {}

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
