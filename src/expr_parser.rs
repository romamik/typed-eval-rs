use crate::expr::*;
use chumsky::{
    cache::{Cache, Cached},
    prelude::*,
};
use std::cell::LazyCell;

pub fn parse_expr(input: &str) -> ParseResult<Expr, Rich<'_, char>> {
    PARSER.with(|parser| parser.get().parse(input))
}

thread_local! {
    static PARSER: LazyCell<Cache<ExprParser>> = LazyCell::new(Cache::default);
}

#[derive(Default)]
struct ExprParser;

impl Cached for ExprParser {
    type Parser<'src> = Box<dyn Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> + 'src>;

    fn make_parser<'src>(self) -> Self::Parser<'src> {
        Box::new(expr_parser())
    }
}

fn expr_parser<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
    recursive(|expr| {
        let num = text::int(10)
            .then(just('.').then(text::int(10).or_not()).or_not())
            .then(
                just('e')
                    .then(just('-').or_not())
                    .then(text::int(10).or_not())
                    .or_not(),
            )
            .to_slice()
            .try_map(|s: &str, span| {
                if s.contains(['.', 'e']) {
                    s.parse()
                        .map(Expr::Float)
                        .map_err(|e| Rich::custom(span, e.to_string()))
                } else {
                    s.parse()
                        .map(Expr::Int)
                        .map_err(|e| Rich::custom(span, e.to_string()))
                }
            });

        let var = text::ident().map(|s: &str| Expr::Var(s.to_owned()));

        let parens = expr.delimited_by(just('('), just(')'));

        let atom = choice((num, var, parens)).padded();

        let field_access = atom.clone().foldl(
            just('.').ignore_then(text::ident()).repeated(),
            |lhs, field: &str| Expr::FieldAccess(Box::new(lhs), field.to_string()),
        );

        let unary = one_of("+-")
            .padded()
            .map(|c| UnOp::from_char(c).unwrap())
            .repeated()
            .foldr(field_access, |op, rhs| Expr::UnOp(op, Box::new(rhs)));

        let product = unary.clone().foldl(
            one_of("*/")
                .padded()
                .map(|c| BinOp::from_char(c).unwrap())
                .then(unary)
                .repeated(),
            |lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)),
        );

        let sum = product.clone().foldl(
            one_of("+-")
                .padded()
                .map(|c| BinOp::from_char(c).unwrap())
                .then(product)
                .repeated(),
            |lhs, (op, rhs)| Expr::BinOp(op, Box::new(lhs), Box::new(rhs)),
        );

        #[allow(clippy::let_and_return)]
        sum
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{BinOp, Expr, UnOp};

    fn parse_ok(input: &str) -> Expr {
        let result = parse_expr(input);
        if result.has_errors() {
            for e in result.errors() {
                eprintln!("Parse error: {e:?}");
            }
            panic!("Failed to parse input: {}", input);
        }
        result.into_output().unwrap()
    }

    #[test]
    fn test_parse_integers_and_floats() {
        assert_eq!(parse_ok("42"), Expr::Int(42));
        assert_eq!(parse_ok("0"), Expr::Int(0));
        assert_eq!(parse_ok("1.1415"), Expr::Float(1.1415));
        assert_eq!(parse_ok("2e3"), Expr::Float(2000.0));
        assert_eq!(parse_ok("2e-3"), Expr::Float(2e-3));
    }

    #[test]
    fn test_parse_variables() {
        assert_eq!(parse_ok("x"), Expr::Var("x".to_string()));
        assert_eq!(parse_ok("foo123"), Expr::Var("foo123".to_string()));
    }

    #[test]
    fn test_parse_parentheses() {
        let expr = parse_ok("(42)");
        assert_eq!(expr, Expr::Int(42));

        let expr2 = parse_ok("((3.5))");
        assert_eq!(expr2, Expr::Float(3.5));
    }

    #[test]
    fn test_parse_unary_operators() {
        let expr = parse_ok("-42");
        assert_eq!(expr, Expr::UnOp(UnOp::Neg, Box::new(Expr::Int(42))));

        let expr2 = parse_ok("+3.5");
        assert_eq!(expr2, Expr::UnOp(UnOp::Plus, Box::new(Expr::Float(3.5))));

        let expr3 = parse_ok("+-3.5");
        assert_eq!(
            expr3,
            Expr::UnOp(
                UnOp::Plus,
                Box::new(Expr::UnOp(UnOp::Neg, Box::new(Expr::Float(3.5))))
            )
        );
    }

    #[test]
    fn test_parse_binary_operators() {
        let expr = parse_ok("1 + 2");
        assert_eq!(
            expr,
            Expr::BinOp(BinOp::Add, Box::new(Expr::Int(1)), Box::new(Expr::Int(2)),)
        );

        let expr2 = parse_ok("3 * 4 + 5");
        assert_eq!(
            expr2,
            Expr::BinOp(
                BinOp::Add,
                Box::new(Expr::BinOp(
                    BinOp::Mul,
                    Box::new(Expr::Int(3)),
                    Box::new(Expr::Int(4))
                )),
                Box::new(Expr::Int(5))
            )
        );
    }

    #[test]
    fn test_parse_field_access() {
        let expr = parse_ok("foo.bar.baz");
        assert_eq!(
            expr,
            Expr::FieldAccess(
                Box::new(Expr::FieldAccess(
                    Box::new(Expr::Var("foo".to_string())),
                    "bar".to_string()
                )),
                "baz".to_string()
            )
        );
    }
}
