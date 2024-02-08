use std::sync::Arc;

use crate::{
    env::Environment,
    eval::Evaluator,
    expr::{ArithInfixOp, CompareInfixOp, Expr, InfixOp},
    val::{Ident, PredefinedFnImpl, Value},
};

mod env;
mod err;
mod eval;
mod expr;
mod instr;
mod val;

fn main() {
    let mut env = Environment::new();
    env.declare(
        Ident("display".to_string()),
        Value::PredefinedFn {
            r#impl: PredefinedFnImpl::new(Arc::new(|args| {
                println!("{:?}", args);
                Ok(Value::Void)
            })),
        },
    );

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
                    Expr::Apply {
                        r#fn: Box::new(Expr::Ident(Ident("display".to_string()))),
                        args: vec![
                            Expr::Ident(Ident("a".to_string())),
                            Expr::Ident(Ident("b".to_string())),
                        ],
                    },
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
            Expr::Return(Box::new(Expr::Ident(Ident("a".to_string())))),
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
            r#fn: Box::new(identity.clone()),
            args: vec![Expr::Ident(Ident("add".to_string()))],
        }),
        args: vec![Expr::Literal(Value::Int(4))],
    };
    // let body = Expr::Body(vec![add, val]);
    let body = Expr::Body(vec![
        fib,
        fib_iter,
        identity,
        Expr::Apply {
            r#fn: Box::new(Expr::Ident(Ident("fib_iter".to_string()))),
            //r#fn: Box::new(Expr::Ident(Ident("identity".to_string()))),
            args: vec![Expr::Literal(Value::Int(19))],
        },
    ]);
    let mut evaluator = Evaluator::new(env);
    let val = evaluator.eval(body);
    println!("Hello, world! {val:?}");
}
