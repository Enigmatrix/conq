use chumsky::prelude::*;

use crate::ast::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let op = |c: &'static str| just(c).padded();

            let let_ = text::keyword("let")
                .padded()
                .ignore_then(text::ident().map(|s: String| Ident(s)))
                .padded()
                .then_ignore(just('=').padded())
                .then(expr.clone())
                .then_ignore(op(";").padded())
                .map(|(name, expr)| Expr::Assign {
                    name,
                    expr: Box::new(expr),
                });

            let int = text::int(10).map(|s: String| Expr::Literal(Value::Int(s.parse().unwrap())));

            let bool = choice((
                text::keyword("true").to(Expr::Literal(Value::Bool(true))),
                text::keyword("false").to(Expr::Literal(Value::Bool(false))),
            ));

            let str_ = none_of('"')
                .repeated()
                .delimited_by(just('"'), just('"'))
                .map(|v: Vec<char>| Expr::Literal(Value::String(v.into_iter().collect())));

            let atom = int
                .or(bool)
                .or(str_)
                .or(inline_expr.delimited_by(just('('), just(')')))
                .or(let_)
                .or(text::ident().map(|s: String| Expr::Ident(Ident(s))))
                .padded();

            // http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
            let unary = choice((op("-").to(UnaryOp::Neg), op("!").to(UnaryOp::Not)))
                .repeated()
                .then(atom)
                .foldr(|op, rhs| Expr::Unary {
                    op,
                    expr: Box::new(rhs),
                });

            // Precendence Block 1
            let product = unary
                .clone()
                .then(
                    choice((
                        op("%").to(ArithBinaryOp::Mod),
                        op("*").to(ArithBinaryOp::Mul),
                        op("/").to(ArithBinaryOp::Div),
                    ))
                    .then(unary)
                    .repeated(),
                )
                .foldl(|lhs, (_op, rhs)| Expr::Binary {
                    op: BinaryOp::Arith(_op),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });

            // Precendence Block 2
            let sum = product
                .clone()
                .then(
                    choice((
                        op("+").to(ArithBinaryOp::Add),
                        op("-").to(ArithBinaryOp::Sub),
                    ))
                    .then(product)
                    .repeated(),
                )
                .foldl(|lhs, (_op, rhs)| Expr::Binary {
                    op: BinaryOp::Arith(_op),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });

            let comp = sum
                .clone()
                .then(
                    choice((
                        op(">").to(CompareBinaryOp::Gt),
                        op("<").to(CompareBinaryOp::Lt),
                        op(">=").to(CompareBinaryOp::Gte),
                        op("<=").to(CompareBinaryOp::Lte),
                        op("==").to(CompareBinaryOp::Eq),
                        op("!=").to(CompareBinaryOp::Neq),
                    ))
                    .then(sum)
                    .repeated(),
                )
                .foldl(|lhs, (_op, rhs)| Expr::Binary {
                    op: BinaryOp::Compare(_op),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });

            let logical = comp
                .clone()
                .then(
                    choice((
                        op("&&").to(LogicalBinaryOp::And),
                        op("||").to(LogicalBinaryOp::Or),
                    ))
                    .then(comp)
                    .repeated(),
                )
                .foldl(|lhs, (_op, rhs)| Expr::Binary {
                    op: BinaryOp::Logical(_op),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
            logical
        });
        inline_expr
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_int() {
        assert_eq!(parser().parse(" 505  "), Ok(Expr::Literal(Value::Int(505))));
    }

    #[test]
    fn parse_neg() {
        assert_eq!(
            parser().parse(" ---505  "),
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
            parser().parse(" 1 + 2 * ( (3 - 4) / 5 ) % 6 "),
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
    fn parse_bool() {
        assert_eq!(
            parser().parse(" true "),
            Ok(Expr::Literal(Value::Bool(true)))
        );
        assert_eq!(
            parser().parse(" false "),
            Ok(Expr::Literal(Value::Bool(false)))
        );
        assert_eq!(
            parser().parse(" ! true "),
            Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Literal(Value::Bool(true)))
            })
        );
    }

    #[test]
    fn parse_string() {
        assert_eq!(
            parser().parse(" \"hello\" "),
            Ok(Expr::Literal(Value::String("hello".to_string())))
        );
        assert_eq!(
            parser().parse(" \"hello\" + \"world\" "),
            Ok(Expr::Binary {
                op: BinaryOp::Arith(ArithBinaryOp::Add), // need to check this in compiler
                lhs: Box::new(Expr::Literal(Value::String("hello".to_string()))),
                rhs: Box::new(Expr::Literal(Value::String("world".to_string()))),
            })
        );
    }

    #[test]
    fn parse_logical_compare() {
        assert_eq!(
            parser().parse(" 1 < 2 && 3 > 4 || false != true"),
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
            parser().parse(" a "),
            Ok(Expr::Ident(Ident("a".to_string())))
        );
        assert_eq!(
            parser().parse(" a + b "),
            Ok(Expr::Binary {
                op: BinaryOp::Arith(ArithBinaryOp::Add),
                lhs: Box::new(Expr::Ident(Ident("a".to_string()))),
                rhs: Box::new(Expr::Ident(Ident("b".to_string()))),
            })
        );
        assert_eq!(
            parser().parse(" let a = 5 ; "),
            Ok(Expr::Assign {
                name: Ident("a".to_string()),
                expr: Box::new(Expr::Literal(Value::Int(5))),
            })
        );
    }
}
