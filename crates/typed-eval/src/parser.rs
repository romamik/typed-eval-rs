use crate::{BinOp, Expr, Span, UnOp, WithSpan};
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
            .try_map(|s: &str, span: SimpleSpan| {
                if s.contains(['.', 'e']) {
                    s.parse()
                        .map(|val: f64| Expr::Float(val.with_span(span)))
                        .map_err(|e| Rich::custom(span, e.to_string()))
                } else {
                    s.parse()
                        .map(|val: i64| Expr::Int(val.with_span(span)))
                        .map_err(|e| Rich::custom(span, e.to_string()))
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

        let parens = expr.clone().delimited_by(just('('), just(')'));

        let atom = choice((num, string, var, parens)).padded();

        enum Postfix {
            Field(String, Span),
            Call(Vec<Expr>, Span),
        }
        let postfix = atom.foldl(
            choice((
                //field access
                just('.').ignore_then((text::ident::<&str, _>()).map_with(
                    |field: &str, e| {
                        Postfix::Field(field.to_string(), e.span().into())
                    },
                )),
                // function call
                expr.separated_by(just(','))
                    .collect::<Vec<Expr>>()
                    .delimited_by(just('('), just(')'))
                    .map_with(|args: Vec<Expr>, extra| {
                        Postfix::Call(args, extra.span().into())
                    }),
            ))
            .repeated(),
            |lhs, postfix| match postfix {
                Postfix::Field(field, span) => {
                    Expr::FieldAccess(Box::new(lhs), field.with_span(span))
                }
                Postfix::Call(args, span) => {
                    Expr::FuncCall(Box::new(lhs), args.with_span(span))
                }
            },
        );

        let unary = one_of::<_, &str, _>("+-")
            .padded()
            .map_with(|c, e| match c {
                '+' => UnOp::Plus.with_span(e.span()),
                '-' => UnOp::Neg.with_span( e.span()),
                _ => panic!("unexpected symbol: one_of should not return anything unexpected"),
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
                    _ => panic!("unexpected symbol: one_of should not return anything unexpected"),
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
                    _ => panic!("unexpected symbol: one_of should not return anything unexpected"),
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
        assert_eq!(parse_ok("42"), Expr::Int(42.test_span()));
        assert_eq!(parse_ok("0"), Expr::Int(0.test_span()));
        assert_eq!(parse_ok("1.1415"), Expr::Float(1.1415.test_span()));
        assert_eq!(parse_ok("2e3"), Expr::Float(2000.0.test_span()));
        assert_eq!(parse_ok("2e-3"), Expr::Float(2e-3.test_span()));
    }

    #[test]
    fn test_parse_strings() {
        assert_eq!(
            parse_ok(r#" "Hello" "#),
            Expr::String("Hello".to_string().test_span())
        );
        assert_eq!(
            parse_ok(r#" "\" world\"" "#),
            Expr::String("\" world\"".to_string().test_span())
        );
        assert_eq!(
            parse_ok(r#" "\n\r\t\\" "#),
            Expr::String("\n\r\t\\".to_string().test_span())
        );
    }

    #[test]
    fn test_parse_variables() {
        assert_eq!(parse_ok("x"), Expr::Var("x".to_string().test_span()));
        assert_eq!(
            parse_ok("foo123"),
            Expr::Var("foo123".to_string().test_span())
        );
    }

    #[test]
    fn test_parse_parentheses() {
        let expr = parse_ok("(42)");
        assert_eq!(expr, Expr::Int(42.test_span()));

        let expr2 = parse_ok("((3.5))");
        assert_eq!(expr2, Expr::Float(3.5.test_span()));
    }

    #[test]
    fn test_parse_unary_operators() {
        let expr = parse_ok("-42");
        assert_eq!(
            expr,
            Expr::UnOp(
                UnOp::Neg.test_span(),
                Box::new(Expr::Int(42.test_span())),
            )
        );

        let expr2 = parse_ok("+3.5");
        assert_eq!(
            expr2,
            Expr::UnOp(
                UnOp::Plus.test_span(),
                Box::new(Expr::Float(3.5.test_span())),
            )
        );

        let expr3 = parse_ok("+-3.5");
        assert_eq!(
            expr3,
            Expr::UnOp(
                UnOp::Plus.test_span(),
                Box::new(Expr::UnOp(
                    UnOp::Neg.test_span(),
                    Box::new(Expr::Float(3.5.test_span())),
                )),
            )
        );
    }

    #[test]
    fn test_parse_binary_operators() {
        let expr = parse_ok("1 + 2");
        assert_eq!(
            expr,
            Expr::BinOp(
                BinOp::Add.test_span(),
                Box::new(Expr::Int(1.test_span())),
                Box::new(Expr::Int(2.test_span())),
            )
        );

        let expr2 = parse_ok("3 * 4 + 5");
        assert_eq!(
            expr2,
            Expr::BinOp(
                BinOp::Add.test_span(),
                Box::new(Expr::BinOp(
                    BinOp::Mul.test_span(),
                    Box::new(Expr::Int(3.test_span())),
                    Box::new(Expr::Int(4.test_span())),
                )),
                Box::new(Expr::Int(5.test_span())),
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
                    Box::new(Expr::Var("foo".to_string().test_span())),
                    "bar".to_string().test_span(),
                )),
                "baz".to_string().test_span(),
            )
        );
    }

    #[test]
    fn test_parse_function_calls() {
        // Simple function call without arguments
        let expr = parse_ok("foo()");
        assert_eq!(
            expr,
            Expr::FuncCall(
                Box::new(Expr::Var("foo".to_string().test_span())),
                vec![].test_span(),
            )
        );

        // Function call with one argument
        let expr2 = parse_ok("bar(42)");
        assert_eq!(
            expr2,
            Expr::FuncCall(
                Box::new(Expr::Var("bar".to_string().test_span())),
                vec![Expr::Int(42.test_span())].test_span(),
            )
        );

        // Function call with multiple arguments
        let expr3 = parse_ok("baz(1, 2, 3)");
        assert_eq!(
            expr3,
            Expr::FuncCall(
                Box::new(Expr::Var("baz".to_string().test_span())),
                vec![
                    Expr::Int(1.test_span()),
                    Expr::Int(2.test_span()),
                    Expr::Int(3.test_span())
                ]
                .test_span(),
            )
        );

        // Nested function calls
        let expr4 = parse_ok("outer(inner(1, 2), 3)");
        assert_eq!(
            expr4,
            Expr::FuncCall(
                Box::new(Expr::Var("outer".to_string().test_span())),
                vec![
                    Expr::FuncCall(
                        Box::new(Expr::Var("inner".to_string().test_span())),
                        vec![
                            Expr::Int(1.test_span()),
                            Expr::Int(2.test_span())
                        ]
                        .test_span(),
                    ),
                    Expr::Int(3.test_span())
                ]
                .test_span(),
            )
        );

        // Method-style call: foo.bar(5)
        let expr5 = parse_ok("foo.bar(5)");
        assert_eq!(
            expr5,
            Expr::FuncCall(
                Box::new(Expr::FieldAccess(
                    Box::new(Expr::Var("foo".to_string().test_span())),
                    "bar".to_string().test_span()
                )),
                vec![Expr::Int(5.test_span())].test_span(),
            )
        );
    }

    #[test]
    fn test_field_access_and_function_call() {
        let expr = parse_ok("foo().field");
        assert_eq!(
            expr,
            Expr::FieldAccess(
                Box::new(Expr::FuncCall(
                    Box::new(Expr::Var("foo".to_string().test_span())),
                    vec![].test_span(),
                )),
                "field".to_string().test_span(),
            )
        );
        let expr = parse_ok("foo.field()");
        assert_eq!(
            expr,
            Expr::FuncCall(
                Box::new(Expr::FieldAccess(
                    Box::new(Expr::Var("foo".to_string().test_span())),
                    "field".to_string().test_span(),
                )),
                vec![].test_span(),
            )
        );
    }

    #[test]
    fn test_spans_match_input_substrings() {
        let input = r#"   foo.bar(1 + -2, "hi\t")   "#;
        let expr = parse_expr(input);
        dbg!(&expr);
        let expr = expr.into_output().unwrap();

        // Recursively walk the expression tree and assert that
        // expr.span() corresponds to the substring of `input`.
        fn check(expr: &Expr, input: &str) {
            let span = expr.span();
            let text = &input[span.start..span.end];

            match expr {
                Expr::Int(val) => assert_eq!(text, &val.to_string()),
                Expr::Float(val) => assert_eq!(text, &val.to_string()),
                Expr::String(val) => {
                    // Just check that substring starts/ends with quotes
                    assert!(
                        text.starts_with('"') && text.ends_with('"'),
                        "Text is not in quotes: {text}",
                    );
                    // We cannot check text for equality because of escape symbols
                    // Check the string lengths
                    assert!(text.len() == 2 || !val.is_empty());
                }
                Expr::Var(val) => assert_eq!(text, &**val),
                Expr::UnOp(_, rhs) => check(rhs, input),
                Expr::BinOp(_, lhs, rhs) => {
                    check(lhs, input);
                    check(rhs, input);
                }
                Expr::FieldAccess(lhs, field) => {
                    check(lhs, input);
                    assert_eq!(
                        &input[field.span().start..field.span().end],
                        &**field
                    );
                }
                Expr::FuncCall(func, args) => {
                    check(func, input);
                    for arg in args.iter() {
                        check(arg, input);
                    }
                }
            }
        }

        check(&expr, input);

        // And the outermost span should cover the entire input
        assert_eq!(&input[expr.span().start..expr.span().end], input.trim());
    }
}
