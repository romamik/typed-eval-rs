use super::*;
use crate::{
    WithSpan,
    expr::{BinOp, Expr, UnOp},
};

fn parse_ok(input: &str) -> Expr {
    let (expr, errors) = parse_expr(input);
    if !errors.is_empty() {
        for e in &*errors {
            eprintln!("Parse error: {e:?}");
        }
        panic!("Failed to parse input: {}", input);
    }
    expr.unwrap()
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
        Expr::UnOp(UnOp::Neg.test_span(), Box::new(Expr::Int(42.test_span())),)
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
                    vec![Expr::Int(1.test_span()), Expr::Int(2.test_span())]
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
    let expr = parse_ok(input);

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
            Expr::InvalidLiteral(_) | Expr::ParseError => {}
        }
    }

    check(&expr, input);

    // And the outermost span should cover the entire input
    assert_eq!(&input[expr.span().start..expr.span().end], input.trim());
}

#[test]
fn test_recover_invalid_parens() {
    let (expr, errors) = parse_expr("2 * (1 + )");

    assert_eq!(
        expr,
        Some(Expr::BinOp(
            BinOp::Mul.test_span(),
            Box::new(Expr::Int(2.test_span())),
            Box::new(Expr::ParseError)
        ))
    );

    assert!(!errors.is_empty());
}

#[test]
fn test_recover_invalid_function_call() {
    let (expr, errors) = parse_expr("func(1, 2 garbage, 3)");

    assert_eq!(
        expr,
        Some(Expr::FuncCall(
            Box::new(Expr::Var("func".to_string().test_span())),
            vec![
                Expr::Int(1.test_span()),
                Expr::Int(2.test_span()),
                Expr::Int(3.test_span()),
            ]
            .test_span()
        ))
    );

    assert!(!errors.is_empty());
}

#[test]
fn test_recover_invalid_function_call_2() {
    let (expr, errors) = parse_expr("func(1, , 3)");

    assert_eq!(
        expr,
        Some(Expr::FuncCall(
            Box::new(Expr::Var("func".to_string().test_span())),
            vec![Expr::ParseError].test_span()
        ))
    );

    assert!(!errors.is_empty());
}
