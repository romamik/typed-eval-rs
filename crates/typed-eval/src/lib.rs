mod compiler;
mod dyn_fn;
mod expr;
mod expr_parser;

pub use compiler::*;
pub use dyn_fn::*;
pub use expr::*;
pub use expr_parser::*;

pub fn eval<Ctx: ExprContext>(input: &str, ctx: &Ctx) -> Result<f64, String> {
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?;
    }

    let compiler = Compiler::<Ctx>::default();
    let compiled_expr = compiler.compile::<f64>(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}

#[cfg(test)]
mod tests {
    use crate::*;

    struct TestContext {
        foo: f64,
        bar: f64,
    }

    impl ExprContext for TestContext {
        fn field_getter(field_name: &str) -> Option<fn(&Self) -> f64> {
            match field_name {
                "foo" => Some(|ctx: &TestContext| ctx.foo),
                "bar" => Some(|ctx: &TestContext| ctx.bar),
                _ => None,
            }
        }
    }

    #[test]
    fn test_eval() {
        let ctx = TestContext { foo: 1.0, bar: 2.5 };
        assert_eq!(eval("(1 + 2) * 3", &ctx), Ok((1.0 + 2.0) * 3.0));
        assert_eq!(
            eval("2 * (foo + bar)", &ctx),
            Ok(2.0 * (ctx.foo + ctx.bar))
        );
    }
}
