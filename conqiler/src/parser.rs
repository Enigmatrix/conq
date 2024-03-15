use chumsky::prelude::*;

use crate::ast::*;

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    filter(|c: &char| c.is_ascii_digit())
        .map(|c| Expr::Literal(Value::Int(c.to_digit(10).unwrap() as i64)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse() {
        let src = "0";
        assert_eq!(parser().parse(src), Ok(Expr::Literal(Value::Int(0))));
    }
}