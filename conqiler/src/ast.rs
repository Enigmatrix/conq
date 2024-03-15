
#[derive(Debug, PartialEq)]
pub enum Ast {
    Stmt(Stmt),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub enum ArithBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub enum CompareBinaryOp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, PartialEq)]
pub enum LogicalBinaryOp {
    Or,
    And,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Arith(ArithBinaryOp),
    Compare(CompareBinaryOp),
    Logical(LogicalBinaryOp),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    /*Float(f64),*/
    Bool(bool),
    String(String),
    Void
}

#[derive(Debug, PartialEq)]
pub struct Ident(String);

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Literal(Value),
    Ident(Ident),
    Assign {
        name: Ident,
        expr: Box<Expr>,
    },
    Body {
        stmts: Vec<Stmt>,
        val: Box<Expr>,
    },
    Cond {
        pred: Box<Expr>,
        conseq: Box<Expr>,
        alt: Box<Expr>,
    },
    Apply {
        r#fn: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Let {
        name: Ident,
        expr: Box<Expr>,
    },
    Break,
    Continue,
    Return(Box<Expr>),
    While {
        pred: Box<Expr>,
        body: Box<Expr>,
    },
    Fn {
        // this is just let...
        name: Ident,
        params: Vec<Ident>,
        expr: Box<Expr>,
    },
}
