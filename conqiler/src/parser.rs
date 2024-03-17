use chumsky::prelude::*;

use crate::ast::*;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Bool(bool),
    Num(i64),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Fn,
    Let,
    Print,
    If,
    Else,
    While,
    Return,
    Break,
    Continue,
}

// impl<'src> fmt::Display for Token {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match self {
//             Token::Bool(x) => write!(f, "{}", x),
//             Token::Num(n) => write!(f, "{}", n),
//             Token::Str(s) => write!(f, "{}", s),
//             Token::Op(s) => write!(f, "{}", s),
//             Token::Ctrl(c) => write!(f, "{}", c),
//             Token::Ident(s) => write!(f, "{}", s),
//             Token::Fn => write!(f, "fn"),
//             Token::Let => write!(f, "let"),
//             Token::Print => write!(f, "print"),
//             Token::If => write!(f, "if"),
//             Token::Else => write!(f, "else"),
//             Token::While => write!(f, "while"),
//             Token::Return => write!(f, "return"),
//             Token::Break => write!(f, "break"),
//             Token::Continue => write!(f, "continue"),
//         }
//     }
// }

// One iterative pass to prevent keyword capture
fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let num = text::int(10).map(|s: String| Token::Num(s.parse().unwrap()));
    let str_ = none_of('"')
        .repeated()
        .delimited_by(just('"'), just('"'))
        .map(|v: Vec<char>| Token::Str(v.into_iter().collect()));

    // gotta be a better way to do this
    let op = choice((
        just("-"),
        just("+"),
        just("*"),
        just("/"),
        just("%"),
        just("="),
        just("=="),
        just("!="),
        just(">="),
        just("<="),
        just(">"),
        just("<"),
        just("&&"),
        just("||"),
        just("!"),
    ))
    .map(|s: &str| Token::Op(s.to_string()));
    let ctrl = one_of("(){};,").map(Token::Ctrl);
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "fn" => Token::Fn,
        "let" => Token::Let,
        "print" => Token::Print,
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
    recursive(|expr| {
        let ident = select! {
            Token::Ident(s) => Ident(s),
        };

        let scoped_block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')));

        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Bool(b) => Value::Bool(b),
                Token::Num(n) => Value::Int(n),
                Token::Str(s) => Value::String(s),
            }
            .map(Expr::Literal);

            let atom = val
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                .or(scoped_block.clone())
                .or(ident.clone().map(Expr::Ident));

            // http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
            let op = choice((
                just(Token::Op("-".to_string())).to(UnaryOp::Neg),
                just(Token::Op("!".to_string())).to(UnaryOp::Not),
            ));
            let unary = op.repeated().then(atom).foldr(|op, rhs| Expr::Unary {
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
            logical
        });

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
            .ignore_then(inline_expr.clone())
            .then(scoped_block.clone())
            .then(just(Token::Else).ignore_then(scoped_block.clone()).or_not())
            .map(|((pred, conseq), alt)| Expr::Cond {
                pred: Box::new(pred),
                conseq: Box::new(conseq),
                alt: Box::new(alt.unwrap_or(Expr::Literal(Value::Void))),
            });

        let while_ = just(Token::While)
            .ignore_then(inline_expr.clone())
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

        let apply = ident
            .clone()
            .then(
                inline_expr
                    .clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .allow_trailing()
                    .collect()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map(|(r#fn, args)| Expr::Apply { r#fn, args });

        // Note: don't use `choice` here as order matters: put less specific parsers
        // lower down the chain
        let expr_ = cond
            .or(assign)
            .or(apply)
            .or(inline_expr);

        // Same here
        let stmt = decl
            .or(while_)
            .or(fn_decl)
            .or(expr_
                .clone()
                .then_ignore(just(Token::Ctrl(';')))
                .map(Stmt::Expr)
            );

        let block = recursive(|block| {
            let block = block.delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')));
            stmt.or(block.clone().map(Stmt::Expr))
                .repeated()
                .then((expr_.or(block)).or_not())
                .map(|(stmts, val)| {
                    if stmts.is_empty() {
                        val.unwrap()
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

macro_rules! parser {
    ($s:expr) => {
        parser().parse(lexer().parse($s).unwrap())
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex() {
        assert_eq!(
            lexer()
                .parse(
                    r#"
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
        "#
                )
                .unwrap(),
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
        assert_eq!(parser!(" 505  "), Ok(Expr::Literal(Value::Int(505))));
        assert_eq!(parser!(" true "), Ok(Expr::Literal(Value::Bool(true))));
        assert_eq!(parser!(" false "), Ok(Expr::Literal(Value::Bool(false))));
        assert_eq!(
            parser!(" ! true "),
            Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Literal(Value::Bool(true)))
            })
        );
        assert_eq!(
            parser!(" \"hello\" "),
            Ok(Expr::Literal(Value::String("hello".to_string())))
        );
    }

    #[test]
    fn parse_neg() {
        assert_eq!(
            parser!(" ---505  "),
            Ok(Expr::Unary {
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
            parser!(" 1 + 2 * ( (3 - 4) / 5 ) % 6 "),
            // sub -> div -> mul -> mod -> add
            Ok(Expr::Binary {
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
            parser!(" 1 < 2 && 3 > 4 || false != true"),
            Ok(Expr::Binary {
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
            parser!("let a = 0; a = 1 ; a + 4"),
            Ok(Expr::Body {
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
            parser!(
                r#"
                let a = {
                    let b = 0;
                    a = 1;
                    a + b
                };
                a
            "#
            ),
            Ok(Expr::Body {
                stmts: vec![Stmt::Let {
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
                },],
                val: Box::new(Expr::Ident(Ident("a".to_string()))),
            })
        );
    }

    #[test]
    fn parse_cond() {
        assert_eq!(
            parser!(
                r#"
                if true {
                    1
                } else {
                    0
                }
            "#
            ),
            Ok(Expr::Cond {
                pred: Box::new(Expr::Literal(Value::Bool(true))),
                conseq: Box::new(Expr::Literal(Value::Int(1))),
                alt: Box::new(Expr::Literal(Value::Int(0))),
            })
        );
    }

    #[test]
    fn parse_while() {
        assert_eq!(
            parser!(
                r#"
                while true {
                    1
                }
            "#
            ),
            Ok(Expr::Body {
                stmts: vec![Stmt::While {
                    pred: Box::new(Expr::Literal(Value::Bool(true))),
                    body: Box::new(Expr::Literal(Value::Int(1))),
                },],
                val: Box::new(Expr::Literal(Value::Void)),
            })
        );
    }

    #[test]
    fn parse_fn_decl_apply() {
        assert_eq!(
            parser!(
                r#"
                fn add(a, b) {
                    a + b
                }
                add(1, 2)
            "#
            ),
            Ok(Expr::Body {
                stmts: vec![Stmt::Fn {
                    name: Ident("add".to_string()),
                    params: vec![Ident("a".to_string()), Ident("b".to_string())],
                    expr: Box::new(Expr::Binary {
                        op: BinaryOp::Arith(ArithBinaryOp::Add),
                        lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                        rhs: Box::new(Expr::Ident(Ident("b".to_string()))),
                    }),
                },],
                val: Box::new(Expr::Apply {
                    r#fn: Ident("add".to_string()),
                    args: vec![Expr::Literal(Value::Int(1)), Expr::Literal(Value::Int(2))],
                }),
            })
        );
    }
}
