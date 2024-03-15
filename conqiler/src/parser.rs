use chumsky::prelude::*;

use crate::ast::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Literal(Value::Int(s.parse().unwrap())))
            .padded();

        let bool = text::keyword("true")
            .map(|_| Expr::Literal(Value::Bool(true)))
            .or(text::keyword("false").map(|_| Expr::Literal(Value::Bool(false))))
            .padded();

        let str_ = none_of('"').repeated()
            .delimited_by(just('"'), just('"'))
            .padded()
            .map(| v: Vec<char>| Expr::Literal(Value::String(v.into_iter().collect())));

        let atom = int
            .or(bool)
            .or(str_)
            .or(expr.delimited_by(just('('), just(')'))).padded();

        let op = |c| just(c).padded();

        let unary = op('-').repeated().then(atom).foldr(|_op, rhs| Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(rhs),
        });

        // Precendence Block 1
        let product = unary
            .clone()
            .then(
                op('*')
                    .to(ArithBinaryOp::Mul)
                    .or(op('/').to(ArithBinaryOp::Div))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Arith(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        // Precendence Block 2
        let sum = product
            .clone()
            .then(
                op('+')
                    .to(ArithBinaryOp::Add)
                    .or(op('-').to(ArithBinaryOp::Sub))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::Binary {
                op: BinaryOp::Arith(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        // let r#let = text::keyword("let")
        //     .ignore_then(ident)
        //     .then_ignore(just('='))
        //     .then(sum.clone())
        //     .then_ignore(just(';'))
        //     .map(|(name, rhs)| Expr::Assign { name: crate::ast::Ident(name), expr: Box::new(rhs) });

        // r#let
        //     // Must be later in the chain than `r#let` to avoid ambiguity
        //     .or(sum)
        //     .padded()
        sum
    });

    expr.then_ignore(end())
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
            parser().parse(" 1 + 2 * (3 - 4) / 5 "),
            Ok(Expr::Binary {
                op: BinaryOp::Arith(ArithBinaryOp::Add),
                lhs: Box::new(Expr::Literal(Value::Int(1))),
                rhs: Box::new(Expr::Binary {
                    op: BinaryOp::Arith(ArithBinaryOp::Div),
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOp::Arith(ArithBinaryOp::Mul),
                        lhs: Box::new(Expr::Literal(Value::Int(2))),
                        rhs: Box::new(Expr::Binary {
                            op: BinaryOp::Arith(ArithBinaryOp::Sub),
                            lhs: Box::new(Expr::Literal(Value::Int(3))),
                            rhs: Box::new(Expr::Literal(Value::Int(4))),
                        }),
                    }),
                    rhs: Box::new(Expr::Literal(Value::Int(5))),
                }),
            })
        );
    }

    #[test]
    fn parse_bool() {
        assert_eq!(parser().parse(" true "), Ok(Expr::Literal(Value::Bool(true))));
        assert_eq!(parser().parse(" false "), Ok(Expr::Literal(Value::Bool(false))));
    }

    #[test]
    fn parse_string() {
        assert_eq!(parser().parse(" \"hello\" "), Ok(Expr::Literal(Value::String("hello".to_string()))));
    }
}
