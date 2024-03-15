use chumsky::prelude::*;

use crate::ast::*;


pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    // let int = text::int(10)
    //     .map(|s: String| Expr::Literal(Value::Int(s.parse().unwrap())))
    //     .padded();

    // int.then_ignore(end())

    let int = text::int(10)
    .map(|s: String| Expr::Literal(Value::Int(s.parse().unwrap())))
    .padded();

    let atom = int;

    let op = |c| just(c).padded();

    let unary = op('-')
        .repeated()
        .then(atom)
        .foldr(|_op, rhs| Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(rhs),
        });

    unary.then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_int() {
        assert_eq!(
            parser().parse(" 505  "),
            Ok(Expr::Literal(Value::Int(505)))
        );
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
}