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

        let unary = one_of("+-")
            .padded()
            .map(|c| UnOp::from_char(c).unwrap())
            .repeated()
            .foldr(atom, |op, rhs| Expr::UnOp(op, Box::new(rhs)));

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
