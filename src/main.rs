/*
 * expr :=
 *
 */

use core::panic;
use std::collections::HashMap;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Bool(bool),
    // Float(f64),
    Int(i64),
    // String(String),
    Fn { params: Vec<Ident>, expr: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ArithInfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CompareInfixOp {
    Gt,
    Lt,
    Gte,
    Lte,
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum LogicalInfixOp {
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum InfixOp {
    Arith(ArithInfixOp),
    Compare(CompareInfixOp),
    Logical(LogicalInfixOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
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
    Body(Vec<Expr>),
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

struct Frame {
    inner: HashMap<Ident, Value>,
}

impl Frame {
    fn new() -> Frame {
        Frame {
            inner: HashMap::new(),
        }
    }

    fn lookup(&self, ident: &Ident) -> Option<&Value> {
        self.inner.get(ident)
    }

    // there should be a cleaner way to do this...
    fn assign(&mut self, ident: Ident, val: Value) -> &Value {
        self.inner.insert(ident.clone(), val);
        self.inner.get(&ident).unwrap()
    }
}

struct Environment {
    frames: Vec<Frame>,
}

impl Environment {
    fn new() -> Environment {
        Environment {
            frames: vec![Frame::new()],
        }
    }

    fn lookup(&self, ident: &Ident) -> Option<&Value> {
        self.frames
            .iter()
            .rev()
            .fold(None, |v, frame| v.or_else(|| frame.lookup(ident)))
    }

    fn assign(&mut self, ident: Ident, val: Value) -> &Value {
        self.frames
            .last_mut()
            .expect("global frame not found")
            .assign(ident, val)
    }

    fn scoped<T>(&mut self, run: impl FnOnce(&mut Self) -> T) -> T {
        self.frames.push(Frame::new());
        let val = run(self);
        self.frames.pop();
        return val;
    }
}

fn eval_unary(op: UnaryOp, val: Value) -> Value {
    match val {
        Value::Bool(b) => {
            if op == UnaryOp::Not {
                Value::Bool(!b)
            } else {
                panic!("invalid op for bool")
            }
        }
        Value::Int(i) => {
            if op == UnaryOp::Neg {
                Value::Int(-i)
            } else {
                panic!("invalid op for int")
            }
        }
        Value::Fn { .. } => panic!("can't use unary op on functions"),
    }
}

fn eval_infix(op: InfixOp, lhs: Value, rhs: Value) -> Value {
    match op {
        InfixOp::Arith(arith) => {
            let (lhs, rhs) = match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => (lhs, rhs),
                _ => panic!("arith requires both to be int"),
            };
            Value::Int(match arith {
                ArithInfixOp::Add => lhs + rhs,
                ArithInfixOp::Sub => lhs - rhs,
                ArithInfixOp::Mul => lhs * rhs,
                ArithInfixOp::Div => lhs / rhs,
            })
        }
        InfixOp::Compare(cmp) => {
            let (lhs, rhs) = match (lhs, rhs) {
                (Value::Int(lhs), Value::Int(rhs)) => (lhs, rhs),
                (Value::Bool(lhs), Value::Bool(rhs)) => {
                    (if lhs { 1 } else { 0 }, if rhs { 1 } else { 0 })
                }
                _ => panic!("cmp requires both to be same type"),
            };
            Value::Bool(match cmp {
                CompareInfixOp::Gt => lhs > rhs,
                CompareInfixOp::Lt => lhs < rhs,
                CompareInfixOp::Gte => lhs >= rhs,
                CompareInfixOp::Lte => lhs <= rhs,
                CompareInfixOp::Eq => lhs == rhs,
                CompareInfixOp::Neq => lhs != rhs,
            })
        }
        InfixOp::Logical(logical) => {
            let (lhs, rhs) = match (lhs, rhs) {
                (Value::Bool(lhs), Value::Bool(rhs)) => (lhs, rhs),
                _ => panic!("logical requires both to be int"),
            };
            Value::Bool(match logical {
                // TODO this is eager...
                LogicalInfixOp::Or => lhs || rhs,
                LogicalInfixOp::And => lhs && rhs,
            })
        }
    }
}

fn eval(expr: Expr, env: &mut Environment) -> Value {
    // TODO make this return a result
    match expr {
        Expr::Ident(ident) => env.lookup(&ident).expect("ident not found").clone(),
        Expr::Literal(val) => val,
        Expr::Unary { op, expr } => eval_unary(op, eval(*expr, env)),
        Expr::Infix { op, lhs, rhs } => eval_infix(op, eval(*lhs, env), eval(*rhs, env)),
        Expr::Let { name, expr } => {
            let val = eval(*expr, env);
            env.assign(name, val).clone()
        }
        Expr::Body(exprs) => env.scoped(|env| {
            exprs
                .into_iter()
                .map(|expr| eval(expr, env))
                .last()
                .expect("body empty")
        }), // sequence is always non-empty
        Expr::Fn { name, params, expr } => {
            let val = Value::Fn { params, expr };
            env.assign(name, val).clone()
        }
        Expr::Apply { r#fn, args } => {
            let r#fn = eval(*r#fn, env);
            if let Value::Fn { params, expr } = r#fn {
                if params.len() != args.len() {
                    panic!("wrong number of arguments to function");
                }
                let args: Vec<_> = args.into_iter().map(|arg| eval(arg, env)).collect();
                let val = env.scoped(|env| {
                    params.into_iter().zip(args).for_each(|(ident, arg)| {
                        env.assign(ident, arg);
                    });
                    eval(*expr, env)
                });
                val
            } else {
                panic!("can't apply on non-func");
            }
        }
    }
}

fn main() {
    let mut env = Environment::new();
    let add = Expr::Fn {
        name: Ident("add".to_string()),
        params: vec![Ident("a".to_string())],
        expr: Box::new(Expr::Infix {
            op: InfixOp::Arith(ArithInfixOp::Add),
            lhs: Box::new(Expr::Literal(Value::Int(1))),
            rhs: Box::new(Expr::Ident(Ident("a".to_string()))),
        }),
    };
    let identity = Expr::Fn {
        name: Ident("identity".to_string()),
        params: vec![Ident("x".to_string())],
        expr: Box::new(Expr::Ident(Ident("x".to_string()))),
    };
    let val = Expr::Apply {
        r#fn: Box::new(Expr::Apply {
            r#fn: Box::new(identity),
            args: vec![Expr::Ident(Ident("add".to_string()))],
        }),
        args: vec![Expr::Literal(Value::Int(4))],
    };
    let body = Expr::Body(vec![add, val]);
    let val = eval(body, &mut env);
    println!("Hello, world! {val:?}");
}
