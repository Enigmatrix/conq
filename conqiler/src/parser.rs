use chumsky::prelude::*;

use crate::ast::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {

    recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Literal(Value::Int(s.parse().unwrap())))
            .padded();

        let atom = int
            .or(expr.delimited_by(just('('), just(')'))).padded();

        let op = |c| just(c).padded();

        let unary = op('-').repeated().then(atom).foldr(|_op, rhs| Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(rhs),
        });

        // Precendence Block 1
        let product = unary
            .clone()
            .then(op('*').to(ArithBinaryOp::Mul)
            .or(op('/').to(ArithBinaryOp::Div))
            .then(unary)
            .repeated())
        .foldl(|lhs, (op, rhs)| Expr::Binary { op: BinaryOp::Arith(op), lhs: Box::new(lhs), rhs: Box::new(rhs) });

        // Precendence Block 2
        let sum = product
            .clone()
            .then(op('+').to(ArithBinaryOp::Add)
            .or(op('-').to(ArithBinaryOp::Sub))
            .then(product)
            .repeated())
        .foldl(|lhs, (op, rhs)| Expr::Binary { op: BinaryOp::Arith(op), lhs: Box::new(lhs), rhs: Box::new(rhs) });

        sum
    })
        .then_ignore(end())
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
    fn test_arith() {
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
}
