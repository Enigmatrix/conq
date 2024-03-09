pub enum Ast {
    Stmt(Stmt),
    Expr(Expr),
}

pub enum ArithBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub enum CompareBinaryOp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
}

pub enum LogicalBinaryOp {
    Or,
    And,
}

pub enum BinaryOp {
    Arith(ArithBinaryOp),
    Compare(CompareBinaryOp),
    Logical(LogicalBinaryOp),
}

pub enum UnaryOp {
    Not,
    Neg,
}

pub enum Type {
    Int,
    /*Float,*/ Bool,
    String,
    Void,
}

pub struct Value {
    typ: Type, // TODO: Add value representation
}

pub struct Ident(String);

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
