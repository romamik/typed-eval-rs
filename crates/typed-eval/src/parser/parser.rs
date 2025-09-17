use crate::{BinOp, Errors, Expr, Spanned, UnOp, WithSpan};
use chumsky::{
    cache::{Cache, Cached},
    prelude::*,
};
use std::cell::LazyCell;

pub fn parse_expr(input: &str) -> (Option<Expr>, Errors) {
    let (expr, errors) = PARSER
        .with(|parser| parser.get().parse(input))
        .into_output_errors();
    (
        expr,
        errors
            .into_iter()
            .map(|e| e.into())
            .collect::<Vec<_>>()
            .into(),
    )
}

thread_local! {
    static PARSER: LazyCell<Cache<ExprParser>> = LazyCell::new(Cache::default);
}

#[derive(Default)]
struct ExprParser;

impl Cached for ExprParser {
    type Parser<'src> = Box<
        dyn Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> + 'src,
    >;

    fn make_parser<'src>(self) -> Self::Parser<'src> {
        Box::new(expr_parser())
    }
}

fn expr_parser<'src>()
-> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char>>> {
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
            .map_with(|s: &str, e| {
                if s.contains(['.', 'e']) {
                    s.parse()
                        .map(|val: f64| Expr::Float(val.with_span(e.span())))
                        .unwrap_or_else(|err| Expr::InvalidLiteral(err.to_string().with_span(e.span())))
                } else {
                    s.parse()
                        .map(|val: i64| Expr::Int(val.with_span(e.span())))
                        .unwrap_or_else(|err| Expr::InvalidLiteral(err.to_string().with_span(e.span())))
                }
            });

        let string = choice((
            none_of::<_, &str, _>("\"\\"),
            just(r#"\\"#).to('\\'),
            just(r#"\""#).to('"'),
            just(r#"\n"#).to('\n'),
            just(r#"\t"#).to('\t'),
            just(r#"\r"#).to('\r'),
        ))
        .repeated()
        .collect::<String>()
        .delimited_by(just('"'), just('"'))
        .map_with(|s, e| Expr::String(s.with_span(e.span())));

        let var = text::ident::<&str, _>().map_with(|s: &str, extra| {
            Expr::Var(s.to_owned().with_span(extra.span()))
        });

        let parens = expr
            .clone()
            .delimited_by(just('('), just(')'))
            .recover_with(via_parser(nested_delimiters(
                '(',
                ')',
                [],
                |_| Expr::ParseError,
            )));

        let atom = choice((num, string, var, parens)).padded();

        enum Postfix {
            Field(Spanned<String>),
            Call(Spanned<Vec<Expr>>),
        }
        let postfix = atom.foldl(
            choice((
                //field access
                just('.').ignore_then((text::ident::<&str, _>()).map_with(
                    |field: &str, e| {
                        Postfix::Field(field.to_string().with_span(e.span()))
                    },
                )),
                // function call
                expr.separated_by(
                    just(',')
                        .padded()
                        .recover_with(skip_then_retry_until(any().ignored(),one_of(",)").ignored()))
                    )
                    .collect::<Vec<Expr>>()
                    .delimited_by(just('('), just(')'))
                    .map_with(|args: Vec<Expr>, extra| {
                        Postfix::Call(args.with_span(extra.span()))
                    }).recover_with(via_parser(nested_delimiters(
                        '(',
                        ')',
                        [],
                        |span: SimpleSpan| Postfix::Call(vec![Expr::ParseError].with_span(span)),
                    )))
            ))
            .repeated(),
            |lhs, postfix| match postfix {
                Postfix::Field(field) => {
                    Expr::FieldAccess(Box::new(lhs), field)
                }
                Postfix::Call(args) => {
                    Expr::FuncCall(Box::new(lhs), args)
                }
            },
        );

        let unary = one_of::<_, &str, _>("+-")
            .padded()
            .map_with(|c, e| match c {
                '+' => UnOp::Plus.with_span(e.span()),
                '-' => UnOp::Neg.with_span( e.span()),
                _ => unreachable!("unexpected symbol: one_of should not return anything unexpected"),
            })
            .repeated()
            .foldr(postfix, |op, rhs| {
                Expr::UnOp(op, Box::new(rhs))
            });

        let product = unary.clone().foldl(
            one_of("*/")
                .padded()
                .map_with(|c, e| match c {
                    '*' => BinOp::Mul.with_span( e.span()),
                    '/' => BinOp::Div.with_span(e.span()),
                    _ => unreachable!("unexpected symbol: one_of should not return anything unexpected"),
                })
                .then(unary)
                .repeated(),
            |lhs, (op, rhs)| {

                Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
            },
        );

        let sum = product.clone().foldl(
            one_of("+-")
                .padded()
                .map_with(|c,e| match c {
                    '+' => BinOp::Add.with_span(e.span()),
                    '-' => BinOp::Sub.with_span(e.span()),
                    _ => unreachable!("unexpected symbol: one_of should not return anything unexpected"),
                })
                .then(product)
                .repeated(),
            |lhs, (op, rhs)| {
                Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
            },
        );

        #[allow(clippy::let_and_return)]
        sum
    }).padded().then_ignore(end())
}
