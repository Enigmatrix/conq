use crate::expr::{Expr, InfixOp, UnaryOp};
use crate::val::Ident;

#[derive(Clone, Debug)]
pub enum Instruction {
    Pop,
    PopEnv,
    UnaryOp(UnaryOp),
    InfixOp(InfixOp),
    Let(Ident),
    Assign(Ident),
    Break,
    Continue,
    Return,
    Call { nargs: usize },
    Mark,
    Cond { conseq: Expr, alt: Expr },
    While { pred: Expr, body: Expr },
}
