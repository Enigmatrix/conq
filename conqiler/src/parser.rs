use chumsky::prelude::*;

use crate::ast::*;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Bool(bool),
    Num(i32),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Fn,
    Let,
    If,
    Else,
    While,
    Return,
    Break,
    Continue,
}

// One iterative pass to prevent keyword capture
fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let num = text::int(10).map(|s: String| Token::Num(s.parse().unwrap()));
    let str_ = none_of('"')
        .repeated()
        .delimited_by(just('"'), just('"'))
        .map(|v: Vec<char>| Token::Str(v.into_iter().collect()));

    // gotta be a better way to do this
    let op = choice((
        just("=="),
        just("!="),
        just(">="),
        just("<="),
        just("&&"),
        just("||"),
        just("-"),
        just("+"),
        just("*"),
        just("/"),
        just("%"),
        just("="),
        just(">"),
        just("<"),
        just("!"),
    ))
    .map(|s: &str| Token::Op(s.to_string()));

    let ctrl = one_of("(){};,").map(Token::Ctrl);
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "fn" => Token::Fn,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        _ => Token::Ident(ident),
    });

    let token = num.or(str_).or(op).or(ctrl).or(ident).padded();

    let comment = just("//").then(none_of('\n').repeated()).padded();

    token.padded_by(comment.repeated()).padded().repeated()
}

pub fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let ident = select! {
        Token::Ident(s) => Ident(s),
    };

    let val = select! {
        Token::Bool(b) => Value::Bool(b),
        Token::Num(n) => Value::Int(n),
        Token::Str(s) => Value::String(s),
    }
    .map(Expr::Literal);

    recursive(|expr| {
        let scoped_block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')));

        let atom = val
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .or(scoped_block.clone())
            .or(ident.clone().map(Expr::Ident));

        let apply = atom
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                    .collect::<Vec<_>>()
                    .repeated(),
            )
            .foldl(|r#fn, args| Expr::Apply {
                r#fn: Box::new(r#fn),
                args,
            });

        // http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
        let op = choice((
            just(Token::Op("-".to_string())).to(UnaryOp::Neg),
            just(Token::Op("!".to_string())).to(UnaryOp::Not),
        ));

        let unary = op.repeated().then(apply).foldr(|op, rhs| Expr::Unary {
            op,
            expr: Box::new(rhs),
        });

        let op = choice((
            just(Token::Op("%".to_string())).to(ArithBinaryOp::Mod),
            just(Token::Op("*".to_string())).to(ArithBinaryOp::Mul),
            just(Token::Op("/".to_string())).to(ArithBinaryOp::Div),
        ));

        let product = unary
            .clone()
            .then(op.then(unary).repeated())
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Arith(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let op = choice((
            just(Token::Op("+".to_string())).to(ArithBinaryOp::Add),
            just(Token::Op("-".to_string())).to(ArithBinaryOp::Sub),
        ));
        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Arith(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let op = choice((
            just(Token::Op(">".to_string())).to(CompareBinaryOp::Gt),
            just(Token::Op("<".to_string())).to(CompareBinaryOp::Lt),
            just(Token::Op(">=".to_string())).to(CompareBinaryOp::Gte),
            just(Token::Op("<=".to_string())).to(CompareBinaryOp::Lte),
            just(Token::Op("==".to_string())).to(CompareBinaryOp::Eq),
            just(Token::Op("!=".to_string())).to(CompareBinaryOp::Neq),
        ));
        let comp = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Compare(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let op = choice((
            just(Token::Op("&&".to_string())).to(LogicalBinaryOp::And),
            just(Token::Op("||".to_string())).to(LogicalBinaryOp::Or),
        ));

        let logical = comp
            .clone()
            .then(op.then(comp).repeated())
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Logical(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let inline_expr = logical;

        let decl = just(Token::Let)
            .ignore_then(ident.clone())
            .then_ignore(just(Token::Op("=".to_string())))
            .then(inline_expr.clone())
            .then_ignore(just(Token::Ctrl(';')))
            .map(|(name, expr)| Stmt::Let {
                name,
                expr: Box::new(expr),
            });

        let assign = ident
            .clone()
            .then_ignore(just(Token::Op("=".to_string())))
            .then(inline_expr.clone())
            .map(|(name, expr)| Expr::Assign {
                name,
                expr: Box::new(expr),
            });

        let cond = just(Token::If)
            .ignore_then(expr.clone())
            .then(scoped_block.clone())
            .then(just(Token::Else).ignore_then(scoped_block.clone()).or_not())
            .map(|((pred, conseq), alt)| Expr::Cond {
                pred: Box::new(pred),
                conseq: Box::new(conseq),
                alt: Box::new(alt.unwrap_or(Expr::Literal(Value::Void))),
            });

        let while_ = just(Token::While)
            .ignore_then(expr.clone())
            .then(scoped_block.clone())
            .map(|(pred, body)| Stmt::While {
                pred: Box::new(pred),
                body: Box::new(body),
            });

        let fn_decl = just(Token::Fn)
            .ignore_then(ident.clone())
            .then(
                ident
                    .clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(scoped_block.clone())
            .map(|((name, params), body)| Stmt::Fn {
                name,
                params,
                expr: Box::new(body),
            });

        let jump = choice((
            just(Token::Return)
                .ignore_then(inline_expr.clone().or_not())
                .map(|expr| Stmt::Return(Box::new(expr.unwrap_or(Expr::Literal(Value::Void))))),
            just(Token::Break).to(Stmt::Break),
            just(Token::Continue).to(Stmt::Continue),
        ))
        .then_ignore(just(Token::Ctrl(';')));

        // Note: order matters: put less specific parsers lower down the chain
        let expr_ = cond.or(assign).or(inline_expr);

        // Same here
        let stmt = jump.or(decl).or(while_).or(fn_decl).or(expr_
            .clone()
            .then_ignore(just(Token::Ctrl(';')))
            .map(Stmt::Expr));

        let block = recursive(|block| {
            let block = block.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')));
            stmt.or(block.clone().map(Stmt::Expr))
                .repeated()
                .then((expr_.or(block)).or_not())
                .map(|(stmts, val)| {
                    if stmts.is_empty() {
                        val.unwrap_or(Expr::Literal(Value::Void))
                    } else {
                        Expr::Body {
                            stmts,
                            val: Box::new(val.unwrap_or(Expr::Literal(Value::Void))),
                        }
                    }
                })
        });
        block
    })
    .then_ignore(end())
}

pub fn parse(s: &str) -> Ast {
    Ast::Expr(parser().parse(lexer().parse(s).unwrap()).unwrap())
}

const EXAMPLE: &str = r#"
    canvas_init();

    let rad_delta = 1;
    let rad_angle = 0;
    let radius = 20;

    while rad_angle <= 2 * math_pi() {
        let x = radius * math_cos(rad_angle);
        let y = radius * math_sin(rad_angle);
        canvas_draw_point(x, y);
        rad_angle = rad_angle + rad_delta;
    }
"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex() {
        assert_eq!(
            lexer().parse(EXAMPLE).unwrap(),
            [
                Token::Ident("canvas_init".to_string()),
                Token::Ctrl('('),
                Token::Ctrl(')'),
                Token::Ctrl(';'),
                Token::Let,
                Token::Ident("rad_delta".to_string()),
                Token::Op("=".to_string()),
                Token::Num(1),
                Token::Ctrl(';'),
                Token::Let,
                Token::Ident("rad_angle".to_string()),
                Token::Op("=".to_string()),
                Token::Num(0),
                Token::Ctrl(';'),
                Token::Let,
                Token::Ident("radius".to_string()),
                Token::Op("=".to_string()),
                Token::Num(20),
                Token::Ctrl(';'),
                Token::While,
                Token::Ident("rad_angle".to_string()),
                Token::Op("<=".to_string()),
                Token::Num(2),
                Token::Op("*".to_string()),
                Token::Ident("math_pi".to_string()),
                Token::Ctrl('('),
                Token::Ctrl(')'),
                Token::Ctrl('{'),
                Token::Let,
                Token::Ident("x".to_string()),
                Token::Op("=".to_string()),
                Token::Ident("radius".to_string()),
                Token::Op("*".to_string()),
                Token::Ident("math_cos".to_string()),
                Token::Ctrl('('),
                Token::Ident("rad_angle".to_string()),
                Token::Ctrl(')'),
                Token::Ctrl(';'),
                Token::Let,
                Token::Ident("y".to_string()),
                Token::Op("=".to_string()),
                Token::Ident("radius".to_string()),
                Token::Op("*".to_string()),
                Token::Ident("math_sin".to_string()),
                Token::Ctrl('('),
                Token::Ident("rad_angle".to_string()),
                Token::Ctrl(')'),
                Token::Ctrl(';'),
                Token::Ident("canvas_draw_point".to_string()),
                Token::Ctrl('('),
                Token::Ident("x".to_string()),
                Token::Ctrl(','),
                Token::Ident("y".to_string()),
                Token::Ctrl(')'),
                Token::Ctrl(';'),
                Token::Ident("rad_angle".to_string()),
                Token::Op("=".to_string()),
                Token::Ident("rad_angle".to_string()),
                Token::Op("+".to_string()),
                Token::Ident("rad_delta".to_string()),
                Token::Ctrl(';'),
                Token::Ctrl('}'),
            ]
        );
    }

    #[test]
    fn parse_atom() {
        assert_eq!(parse(" 505  "), Ast::Expr(Expr::Literal(Value::Int(505))));
        assert_eq!(parse(" true "), Ast::Expr(Expr::Literal(Value::Bool(true))));
        assert_eq!(
            parse(" false "),
            Ast::Expr(Expr::Literal(Value::Bool(false)))
        );
        assert_eq!(
            parse(" ! true "),
            Ast::Expr(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Literal(Value::Bool(true)))
            })
        );
        assert_eq!(
            parse(" \"hello\" "),
            Ast::Expr(Expr::Literal(Value::String("hello".to_string())))
        );
    }

    #[test]
    fn parse_neg() {
        assert_eq!(
            parse(" ---505  "),
            Ast::Expr(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(Expr::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr::Literal(Value::Int(505))),
                    }),
                })
            })
        );
    }

    #[test]
    fn parse_arith() {
        assert_eq!(
            parse(" 1 + 2 * ( (3 - 4) / 5 ) % 6 "),
            // sub -> div -> mul -> mod -> add
            Ast::Expr(Expr::Binary {
                op: BinaryOp::Arith(ArithBinaryOp::Add),
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Binary {
                    op: BinaryOp::Arith(ArithBinaryOp::Mod),
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOp::Arith(ArithBinaryOp::Mul),
                        lhs: Box::new(Expr::Literal(Value::Int(2))),
                        rhs: Box::new(Expr::Binary {
                            op: BinaryOp::Arith(ArithBinaryOp::Div),
                            lhs: Box::new(Expr::Binary {
                                op: BinaryOp::Arith(ArithBinaryOp::Sub),
                                lhs: Box::new(Expr::Literal(Value::Int(3))),
                                rhs: Box::new(Expr::Literal(Value::Int(4))),
                            }),
                            rhs: Box::new(Expr::Literal(Value::Int(5))),
                        }),
                    }),
                    rhs: Box::new(Expr::Literal(Value::Int(6))),
                }),
            })
        );
    }

    #[test]
    fn parse_logical_compare() {
        assert_eq!(
            parse(" 1 < 2 && 3 > 4 || false != true"),
            Ast::Expr(Expr::Binary {
                op: BinaryOp::Logical(LogicalBinaryOp::Or),
                lhs: Box::new(Expr::Binary {
                    op: BinaryOp::Logical(LogicalBinaryOp::And),
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOp::Compare(CompareBinaryOp::Lt),
                        lhs: Box::new(Expr::Literal(Value::Int(1))),
                        rhs: Box::new(Expr::Literal(Value::Int(2))),
                    }),
                    rhs: Box::new(Expr::Binary {
                        op: BinaryOp::Compare(CompareBinaryOp::Gt),
                        lhs: Box::new(Expr::Literal(Value::Int(3))),
                        rhs: Box::new(Expr::Literal(Value::Int(4))),
                    }),
                }),
                rhs: Box::new(Expr::Binary {
                    op: BinaryOp::Compare(CompareBinaryOp::Neq),
                    lhs: Box::new(Expr::Literal(Value::Bool(false))),
                    rhs: Box::new(Expr::Literal(Value::Bool(true))),
                }),
            })
        );
    }

    #[test]
    fn parse_ident_let() {
        assert_eq!(
            parse("let a = 0; a = 1 ; a + 4"),
            Ast::Expr(Expr::Body {
                stmts: vec![
                    Stmt::Let {
                        name: Ident("a".to_string()),
                        expr: Box::new(Expr::Literal(Value::Int(0))),
                    },
                    Stmt::Expr(Expr::Assign {
                        name: Ident("a".to_string()),
                        expr: Box::new(Expr::Literal(Value::Int(1))),
                    }),
                ],
                val: Box::new(Expr::Binary {
                    op: BinaryOp::Arith(ArithBinaryOp::Add),
                    lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                    rhs: Box::new(Expr::Literal(Value::Int(4))),
                }),
            })
        );
    }

    #[test]
    fn parse_block() {
        assert_eq!(
            parse(
                r#"
                {
                    1 + 2
                }
                let a = {
                    let b = 0;
                    a = 1;
                    a + b
                };
                a
            "#
            ),
            Ast::Expr(Expr::Body {
                stmts: vec![
                    Stmt::Expr(Expr::Binary {
                        op: BinaryOp::Arith(ArithBinaryOp::Add),
                        lhs: Box::new(Expr::Literal(Value::Int(1))),
                        rhs: Box::new(Expr::Literal(Value::Int(2))),
                    }),
                    Stmt::Let {
                        name: Ident("a".to_string()),
                        expr: Box::new(Expr::Body {
                            stmts: vec![
                                Stmt::Let {
                                    name: Ident("b".to_string()),
                                    expr: Box::new(Expr::Literal(Value::Int(0))),
                                },
                                Stmt::Expr(Expr::Assign {
                                    name: Ident("a".to_string()),
                                    expr: Box::new(Expr::Literal(Value::Int(1))),
                                }),
                            ],
                            val: Box::new(Expr::Binary {
                                op: BinaryOp::Arith(ArithBinaryOp::Add),
                                lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                                rhs: Box::new(Expr::Ident(Ident("b".to_string()))),
                            }),
                        }),
                    },
                ],
                val: Box::new(Expr::Ident(Ident("a".to_string()))),
            })
        );
    }

    #[test]
    fn parse_cond() {
        assert_eq!(
            parse(
                r#"
                if true {
                    1
                } else {
                    0
                }
            "#
            ),
            Ast::Expr(Expr::Cond {
                pred: Box::new(Expr::Literal(Value::Bool(true))),
                conseq: Box::new(Expr::Literal(Value::Int(1))),
                alt: Box::new(Expr::Literal(Value::Int(0))),
            })
        );
    }

    #[test]
    fn parse_while() {
        assert_eq!(
            parse(
                r#"
                while test(true) == 1 {
                    1
                }
            "#
            ),
            Ast::Expr(Expr::Body {
                stmts: vec![Stmt::While {
                    pred: Box::new(Expr::Binary {
                        op: BinaryOp::Compare(CompareBinaryOp::Eq),
                        lhs: Box::new(Expr::Apply {
                            r#fn: Box::new(Expr::Ident(Ident("test".to_string()))),
                            args: vec![Expr::Literal(Value::Bool(true))],
                        }),
                        rhs: Box::new(Expr::Literal(Value::Int(1))),
                    }),
                    body: Box::new(Expr::Literal(Value::Int(1))),
                },],
                val: Box::new(Expr::Literal(Value::Void)),
            })
        );
    }

    #[test]
    fn parse_fn_apply() {
        assert_eq!(
            parse(
                r#"
                fn add(a, b) {
                    fn add2 (a, b) {
                        let c = 1;
                        a + b
                    }
                    return add2(a, b);
                }
                add(1, 2)()
            "#
            ),
            Ast::Expr(Expr::Body {
                stmts: vec![Stmt::Fn {
                    name: Ident("add".to_string()),
                    params: vec![Ident("a".to_string()), Ident("b".to_string())],
                    expr: Box::new(Expr::Body {
                        stmts: vec![
                            Stmt::Fn {
                                name: Ident("add2".to_string()),
                                params: vec![Ident("a".to_string()), Ident("b".to_string())],
                                expr: Box::new(Expr::Body {
                                    stmts: vec![Stmt::Let {
                                        name: Ident("c".to_string()),
                                        expr: Box::new(Expr::Literal(Value::Int(1))),
                                    },],
                                    val: Box::new(Expr::Binary {
                                        op: BinaryOp::Arith(ArithBinaryOp::Add),
                                        lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                                        rhs: Box::new(Expr::Ident(Ident("b".to_string()))),
                                    }),
                                }),
                            },
                            Stmt::Return(Box::new(Expr::Apply {
                                r#fn: Box::new(Expr::Ident(Ident("add2".to_string()))),
                                args: vec![
                                    Expr::Ident(Ident("a".to_string())),
                                    Expr::Ident(Ident("b".to_string())),
                                ],
                            })),
                        ],
                        val: Box::new(Expr::Literal(Value::Void)),
                    }),
                }],
                val: Box::new(Expr::Apply {
                    r#fn: Box::new(Expr::Apply {
                        r#fn: Box::new(Expr::Ident(Ident("add".to_string()))),
                        args: vec![Expr::Literal(Value::Int(1)), Expr::Literal(Value::Int(2))],
                    }),
                    args: vec![Expr::Literal(Value::Void)],
                }),
            })
        );
    }

    #[test]
    fn parse_example() {
        assert_eq!(
            parse(EXAMPLE),
            Ast::Expr(Expr::Body {
                stmts: vec![
                    Stmt::Expr(Expr::Apply {
                        r#fn: Box::new(Expr::Ident(Ident("canvas_init".to_string()))),
                        args: vec![Expr::Literal(Value::Void)],
                    }),
                    Stmt::Let {
                        name: Ident("rad_delta".to_string()),
                        expr: Box::new(Expr::Literal(Value::Int(1))),
                    },
                    Stmt::Let {
                        name: Ident("rad_angle".to_string()),
                        expr: Box::new(Expr::Literal(Value::Int(0))),
                    },
                    Stmt::Let {
                        name: Ident("radius".to_string()),
                        expr: Box::new(Expr::Literal(Value::Int(20))),
                    },
                    Stmt::While {
                        pred: Box::new(Expr::Binary {
                            op: BinaryOp::Compare(CompareBinaryOp::Lte),
                            lhs: Box::new(Expr::Ident(Ident("rad_angle".to_string()))),
                            rhs: Box::new(Expr::Binary {
                                op: BinaryOp::Arith(ArithBinaryOp::Mul),
                                lhs: Box::new(Expr::Literal(Value::Int(2))),
                                rhs: Box::new(Expr::Apply {
                                    r#fn: Box::new(Expr::Ident(Ident("math_pi".to_string()))),
                                    args: vec![Expr::Literal(Value::Void)],
                                }),
                            }),
                        }),
                        body: Box::new(Expr::Body {
                            stmts: vec![
                                Stmt::Let {
                                    name: Ident("x".to_string()),
                                    expr: Box::new(Expr::Binary {
                                        op: BinaryOp::Arith(ArithBinaryOp::Mul),
                                        lhs: Box::new(Expr::Ident(Ident("radius".to_string()))),
                                        rhs: Box::new(Expr::Apply {
                                            r#fn: Box::new(Expr::Ident(Ident(
                                                "math_cos".to_string()
                                            ))),
                                            args: vec![Expr::Ident(Ident("rad_angle".to_string()))],
                                        }),
                                    }),
                                },
                                Stmt::Let {
                                    name: Ident("y".to_string()),
                                    expr: Box::new(Expr::Binary {
                                        op: BinaryOp::Arith(ArithBinaryOp::Mul),
                                        lhs: Box::new(Expr::Ident(Ident("radius".to_string()))),
                                        rhs: Box::new(Expr::Apply {
                                            r#fn: Box::new(Expr::Ident(Ident(
                                                "math_sin".to_string()
                                            ))),
                                            args: vec![Expr::Ident(Ident("rad_angle".to_string()))],
                                        }),
                                    }),
                                },
                                Stmt::Expr(Expr::Apply {
                                    r#fn: Box::new(Expr::Ident(Ident(
                                        "canvas_draw_point".to_string()
                                    ))),
                                    args: vec![
                                        Expr::Ident(Ident("x".to_string())),
                                        Expr::Ident(Ident("y".to_string())),
                                    ],
                                }),
                                Stmt::Expr(Expr::Assign {
                                    name: Ident("rad_angle".to_string()),
                                    expr: Box::new(Expr::Binary {
                                        op: BinaryOp::Arith(ArithBinaryOp::Add),
                                        lhs: Box::new(Expr::Ident(Ident("rad_angle".to_string()))),
                                        rhs: Box::new(Expr::Ident(Ident("rad_delta".to_string()))),
                                    }),
                                }),
                            ],
                            val: Box::new(Expr::Literal(Value::Void)),
                        }),
                    },
                ],
                val: Box::new(Expr::Literal(Value::Void)),
            })
        );
    }
}
