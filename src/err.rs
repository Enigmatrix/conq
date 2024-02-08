use crate::val::{Ident, Value};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Fn,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EvalError {
    TypeError {
        expected: Type,
        actual: Value,
    },
    IncorrectArgumentLength {
        r#fn: Value,
        expected: usize,
        actual: usize,
    },
    IdentNotFound {
        ident: Ident,
    },
}
