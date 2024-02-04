/*
 * expr :=
 *
 */

use std::collections::HashMap;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Ident(String);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    Void,
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
    Assign {
        name: Ident,
        expr: Box<Expr>,
    },
    Body(Vec<Expr>),
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

    fn lookup(&self, ident: &Ident) -> Option<Value> {
        self.frames
            .iter()
            .rev()
            .fold(None, |v, frame| v.or_else(|| frame.lookup(ident)))
            .cloned()
    }

    fn declare(&mut self, ident: Ident, val: Value) -> Value {
        self.frames
            .last_mut()
            .expect("global frame not found") // literally not possible to occur
            .assign(ident, val)
            .clone()
    }

    fn assign(&mut self, ident: Ident, val: Value) -> Result<Value, EvalError> {
        let decl_frame = self
            .frames
            .iter_mut()
            .rev()
            .skip_while(|frame| frame.lookup(&ident).is_none())
            .next();
        if let Some(decl_frame) = decl_frame {
            Ok(decl_frame.assign(ident, val).clone())
        } else {
            Err(EvalError::IdentNotFound { ident })
        }
    }

    fn scoped<T>(&mut self, run: impl FnOnce(&mut Self) -> T) -> T {
        self.frames.push(Frame::new());
        let val = run(self);
        self.frames.pop();
        return val;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum ControlFlow {
    Break,
    Continue,
    // Return
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum EvalError {
    TypeError {
        expr: Expr,
        expected: String,
        actual: String,
    },
    IncorrectArgumentLength {
        expr: Expr,
        expected: usize,
        actual: usize,
    },
    IdentNotFound {
        ident: Ident,
    },
    ControlFlow(ControlFlow),
}

fn eval_unary(op: UnaryOp, expr: Expr, env: &mut Environment) -> Result<Value, EvalError> {
    let val = eval(expr.clone(), env)?;
    match val {
        Value::Bool(b) => {
            if op == UnaryOp::Not {
                Ok(Value::Bool(!b))
            } else {
                Err(EvalError::TypeError {
                    expr,
                    expected: "Bool".to_string(),
                    actual: format!("{val:?}"),
                })
            }
        }
        Value::Int(i) => {
            if op == UnaryOp::Neg {
                Ok(Value::Int(-i))
            } else {
                Err(EvalError::TypeError {
                    expr,
                    expected: "Int".to_string(),
                    actual: format!("{val:?}"),
                })
            }
        }
        Value::Fn { .. } | Value::Void => Err(EvalError::TypeError {
            expr,
            expected: "Int or Bool".to_string(),
            actual: format!("{val:?}"),
        }),
    }
}

fn eval_infix(
    op: InfixOp,
    lhs_expr: Expr,
    rhs_expr: Expr,
    env: &mut Environment,
) -> Result<Value, EvalError> {
    match op {
        InfixOp::Arith(arith) => {
            let lhs = eval(lhs_expr.clone(), env)?;
            let rhs = eval(rhs_expr.clone(), env)?;
            let (lhs, rhs) = match (lhs.clone(), rhs.clone()) {
                (Value::Int(lhs), Value::Int(rhs)) => (lhs, rhs),
                _ => {
                    return Err(EvalError::TypeError {
                        expr: Expr::Body(vec![lhs_expr, rhs_expr]),
                        expected: "both exprs should be Int".to_string(),
                        actual: format!("{lhs:?}, {rhs:?}"),
                    })
                }
            };
            Ok(Value::Int(match arith {
                ArithInfixOp::Add => lhs + rhs,
                ArithInfixOp::Sub => lhs - rhs,
                ArithInfixOp::Mul => lhs * rhs,
                ArithInfixOp::Div => lhs / rhs,
            }))
        }
        InfixOp::Compare(cmp) => {
            let lhs = eval(lhs_expr.clone(), env)?;
            let rhs = eval(rhs_expr.clone(), env)?;
            let (lhs, rhs) = match (lhs.clone(), rhs.clone()) {
                (Value::Int(lhs), Value::Int(rhs)) => (lhs, rhs),
                (Value::Bool(lhs), Value::Bool(rhs)) => {
                    (if lhs { 1 } else { 0 }, if rhs { 1 } else { 0 })
                }
                _ => {
                    return Err(EvalError::TypeError {
                        expr: Expr::Body(vec![lhs_expr, rhs_expr]),
                        expected: "both exprs should be Int".to_string(),
                        actual: format!("{lhs:?}, {rhs:?}"),
                    })
                }
            };
            Ok(Value::Bool(match cmp {
                CompareInfixOp::Gt => lhs > rhs,
                CompareInfixOp::Lt => lhs < rhs,
                CompareInfixOp::Gte => lhs >= rhs,
                CompareInfixOp::Lte => lhs <= rhs,
                CompareInfixOp::Eq => lhs == rhs,
                CompareInfixOp::Neq => lhs != rhs,
            }))
        }
        InfixOp::Logical(logical) => {
            fn to_bool(expr: Expr, env: &mut Environment) -> Result<bool, EvalError> {
                let v = eval(expr.clone(), env)?;
                match v {
                    Value::Bool(b) => Ok(b),
                    _ => Err(EvalError::TypeError {
                        expr: expr,
                        expected: "Bool".to_string(),
                        actual: format!("{v:?}"),
                    }),
                }
            }

            Ok(Value::Bool(match logical {
                LogicalInfixOp::Or => to_bool(lhs_expr, env)? || to_bool(rhs_expr, env)?,
                LogicalInfixOp::And => to_bool(lhs_expr, env)? && to_bool(rhs_expr, env)?,
            }))
        }
    }
}

fn eval(expr: Expr, env: &mut Environment) -> Result<Value, EvalError> {
    Ok(match expr {
        Expr::Ident(ident) => env
            .lookup(&ident)
            .ok_or(EvalError::IdentNotFound { ident })?
            .clone(),
        Expr::Literal(val) => val,
        Expr::Unary { op, expr } => eval_unary(op, *expr, env)?,
        Expr::Infix { op, lhs, rhs } => eval_infix(op, *lhs, *rhs, env)?,
        Expr::Let { name, expr } => {
            let val = eval(*expr, env)?;
            env.declare(name, val)
        }
        Expr::Assign { name, expr } => {
            let val = eval(*expr, env)?;
            env.assign(name, val)?
        }
        Expr::Body(exprs) => env.scoped(|env| {
            exprs
                .into_iter()
                .map(|expr| eval(expr, env))
                .collect::<Result<Vec<_>, _>>()
                .map(|vals| vals.last().cloned().unwrap_or(Value::Void))
        })?,
        Expr::Fn { name, params, expr } => {
            let val = Value::Fn { params, expr };
            env.declare(name, val)
        }
        Expr::Apply { r#fn, args } => {
            let fn_val = eval(*r#fn.clone(), env)?;
            if let Value::Fn { params, expr } = fn_val.clone() {
                if params.len() != args.len() {
                    return Err(EvalError::IncorrectArgumentLength {
                        expr: *r#fn,
                        expected: args.len(),
                        actual: args.len(),
                    });
                }
                let args = args
                    .into_iter()
                    .map(|arg| eval(arg, env))
                    .collect::<Result<Vec<_>, _>>()?;
                let val = env.scoped(|env| {
                    params.into_iter().zip(args).for_each(|(ident, arg)| {
                        env.declare(ident, arg);
                    });
                    eval(*expr, env)
                })?;
                val
            } else {
                return Err(EvalError::TypeError {
                    expr: *r#fn,
                    expected: "Fn".to_string(),
                    actual: format!("{fn_val:?}"),
                });
            }
        }
        Expr::Cond { pred, conseq, alt } => {
            if eval(*pred, env)? == Value::Bool(true) {
                eval(*conseq, env)?
            } else {
                eval(*alt, env)?
            }
        }
        Expr::While { pred, body } => {
            let mut ret_value = Value::Void;
            while eval(*pred.clone(), env)? == Value::Bool(true) {
                let result = eval(*body.clone(), env);
                match result {
                    Ok(value) => ret_value = value,
                    Err(EvalError::ControlFlow(ControlFlow::Break)) => break,
                    Err(EvalError::ControlFlow(ControlFlow::Continue)) => continue,
                    Err(err) => Err(err)?,
                }
            }
            ret_value
        }
    })
}

fn main() {
    let mut env = Environment::new();

    let fib = Expr::Fn {
        name: Ident("fib".to_string()),
        params: vec![Ident("n".to_string())],
        expr: Box::new(Expr::Cond {
            pred: Box::new(Expr::Infix {
                op: InfixOp::Compare(CompareInfixOp::Eq),
                lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                rhs: Box::new(Expr::Literal(Value::Int(0))),
            }),
            conseq: Box::new(Expr::Literal(Value::Int(0))),
            alt: Box::new(Expr::Cond {
                pred: Box::new(Expr::Infix {
                    op: InfixOp::Compare(CompareInfixOp::Eq),
                    lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                    rhs: Box::new(Expr::Literal(Value::Int(1))),
                }),
                conseq: Box::new(Expr::Literal(Value::Int(1))),
                alt: Box::new(Expr::Infix {
                    op: InfixOp::Arith(ArithInfixOp::Add),
                    lhs: Box::new(Expr::Apply {
                        r#fn: Box::new(Expr::Ident(Ident("fib".to_string()))),
                        args: vec![Expr::Infix {
                            op: InfixOp::Arith(ArithInfixOp::Sub),
                            lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                            rhs: Box::new(Expr::Literal(Value::Int(1))),
                        }],
                    }),
                    rhs: Box::new(Expr::Apply {
                        r#fn: Box::new(Expr::Ident(Ident("fib".to_string()))),
                        args: vec![Expr::Infix {
                            op: InfixOp::Arith(ArithInfixOp::Sub),
                            lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                            rhs: Box::new(Expr::Literal(Value::Int(2))),
                        }],
                    }),
                }),
            }),
        }),
    };
    // let a = 0;
    // let b = 1;
    // while (n > 0) {
    //    let c = a + b;
    //    a = b;
    //    b = c;
    //    n = n - 1; }
    // a

    let fib_iter = Expr::Fn {
        name: Ident("fib_iter".to_string()),
        params: vec![Ident("n".to_string())],
        expr: Box::new(Expr::Body(vec![
            Expr::Let {
                name: Ident("a".to_string()),
                expr: Box::new(Expr::Literal(Value::Int(0))),
            },
            Expr::Let {
                name: Ident("b".to_string()),
                expr: Box::new(Expr::Literal(Value::Int(1))),
            },
            Expr::While {
                pred: Box::new(Expr::Infix {
                    op: InfixOp::Compare(CompareInfixOp::Gt),
                    lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                    rhs: Box::new(Expr::Literal(Value::Int(0))),
                }),
                body: Box::new(Expr::Body(vec![
                    Expr::Let {
                        name: Ident("c".to_string()),
                        expr: Box::new(Expr::Infix {
                            op: InfixOp::Arith(ArithInfixOp::Add),
                            lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                            rhs: Box::new(Expr::Ident(Ident("b".to_string()))),
                        }),
                    },
                    Expr::Assign {
                        name: Ident("a".to_string()),
                        expr: Box::new(Expr::Ident(Ident("b".to_string()))),
                    },
                    Expr::Assign {
                        name: Ident("b".to_string()),
                        expr: Box::new(Expr::Ident(Ident("c".to_string()))),
                    },
                    Expr::Assign {
                        name: Ident("n".to_string()),
                        expr: Box::new(Expr::Infix {
                            op: InfixOp::Arith(ArithInfixOp::Sub),
                            lhs: Box::new(Expr::Ident(Ident("n".to_string()))),
                            rhs: Box::new(Expr::Literal(Value::Int(1))),
                        }),
                    },
                ])),
            },
            Expr::Ident(Ident("a".to_string())),
        ])),
    };

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
    // let body = Expr::Body(vec![add, val]);
    let body = Expr::Body(vec![
        fib,
        fib_iter,
        Expr::Apply {
            r#fn: Box::new(Expr::Ident(Ident("fib_iter".to_string()))),
            args: vec![Expr::Literal(Value::Int(19))],
        },
    ]);
    let val = eval(body, &mut env);
    println!("Hello, world! {val:?}");
}
