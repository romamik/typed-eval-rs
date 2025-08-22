use std::{cell::Cell, rc::Rc};

use typed_eval::{Compiler, SupportedType, parse_expr};

#[derive(Debug, SupportedType)]
struct Foo {
    a: i64,
    b: f64,
    str: String,
}

#[derive(Debug, SupportedType)]
struct Bar {
    c: i64,
    foo: Foo,
}

fn make_ctx() -> Bar {
    Bar {
        foo: Foo {
            a: 10,
            b: 2.5,
            str: "hello".into(),
        },
        c: 3,
    }
}

#[test]
fn test_simple_addition() {
    let compiler = Compiler::<()>::new();
    let expr = parse_expr("1 + 2").unwrap();
    let compiled = compiler.compile::<i64>(&expr).unwrap();
    let result = compiled.call(&());
    assert_eq!(result, 3);
}

#[test]
fn test_field_access_and_addition() {
    let compiler = Compiler::new();
    let expr = parse_expr("foo.a + c").unwrap();
    let compiled = compiler.compile::<i64>(&expr).unwrap();
    let ctx = make_ctx();
    let result = compiled.call(&ctx);
    assert_eq!(result, 10 + 3);
}

#[test]
fn test_mixed_types_multiplication() {
    let compiler = Compiler::<Bar>::new();
    let expr = parse_expr("foo.a * foo.b").unwrap();
    let compiled = compiler.compile::<f64>(&expr).unwrap();
    let ctx = make_ctx();
    let result = compiled.call(&ctx);
    assert_eq!(result, 10.0 * 2.5);
}

#[test]
fn test_strings() {
    let compiler = Compiler::<Bar>::new();
    let expr = parse_expr(r#" foo.str + "-" + foo.b/2 + "-world\n" "#).unwrap();
    let compiled = compiler.compile::<String>(&expr).unwrap();
    let ctx = make_ctx();
    let result = compiled.call(&ctx);
    assert_eq!(result, "hello-1.25-world\n");
}

#[test]
fn test_complex_expression() {
    let compiler = Compiler::<Bar>::new();
    let expr = parse_expr("(foo.a * foo.b - c) * 2.0").unwrap();
    let compiled = compiler.compile::<f64>(&expr).unwrap();
    let ctx = make_ctx();
    let result = compiled.call(&ctx);
    assert_eq!(result, (10.0 * 2.5 - 3.0) * 2.0);
}

#[test]
fn test_function_0_and_1_args() {
    #[derive(SupportedType)]
    struct Ctx {
        f0: Box<dyn Fn() -> String>,
        f1: Box<dyn Fn(i64) -> String>,
        i: i64,
    }

    let expr = parse_expr(r#"f0() + "," + f1(i)"#).unwrap();
    let compiler = Compiler::new();
    let compiled = compiler.compile::<String>(&expr).unwrap();

    let mut ctx = Ctx {
        f0: Box::new(move || "Hello world".to_string()),
        f1: Box::new(|a| format!("f1({a})")),
        i: 0,
    };
    let result = compiled.call(&ctx);
    assert_eq!(result, "Hello world,f1(0)");
    ctx.i = 2;
    let result = compiled.call(&ctx);
    assert_eq!(result, "Hello world,f1(2)");
}

#[test]
fn test_function_2_args() {
    #[derive(SupportedType)]
    struct Ctx {
        f: Box<dyn Fn(i64, i64) -> i64>,
        i: i64,
    }

    let expr = parse_expr(r#"f(i, 2)"#).unwrap();
    let compiler = Compiler::new();
    let compiled = compiler.compile::<i64>(&expr).unwrap();

    let mut ctx = Ctx {
        f: Box::new(move |a, b| a * b),
        i: 0,
    };
    let result = compiled.call(&ctx);
    assert_eq!(result, 0);
    ctx.i = 12;
    let result = compiled.call(&ctx);
    assert_eq!(result, 24);
}
