use crate::val::{Ident, Value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithInfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompareInfixOp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicalInfixOp {
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InfixOp {
    Arith(ArithInfixOp),
    Compare(CompareInfixOp),
    Logical(LogicalInfixOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(Ident),
    Literal(Value),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Let {
        name: Ident,
        expr: Box<Expr>,
    },
    Assign {
        name: Ident,
        expr: Box<Expr>,
    },
    Body(Vec<Expr>),
    Break,
    Continue,
    Return(Box<Expr>),
    Cond {
        pred: Box<Expr>,
        conseq: Box<Expr>,
        alt: Box<Expr>,
    },
    While {
        pred: Box<Expr>,
        body: Box<Expr>,
    },
    Fn {
        name: Ident,
        params: Vec<Ident>,
        expr: Box<Expr>,
    }, // this is just Let with a Fn val isn't it?
    Apply {
        r#fn: Box<Expr>,
        args: Vec<Expr>,
    },
}
