mod expr;
mod expr_parser;

pub use expr::*;
pub use expr_parser::*;

pub trait ExprContext: 'static {
    fn field_getter(field_name: &str) -> Option<fn(&Self) -> f64>;
}

pub type CompiledExpr<Ctx> = Box<dyn Fn(&Ctx) -> f64>;

pub fn compile_expr<Ctx: ExprContext>(expr: &Expr) -> Result<CompiledExpr<Ctx>, String> {
    Ok(match expr {
        &Expr::Int(val) => Box::new(move |_ctx| val as f64),
        &Expr::Float(val) => Box::new(move |_ctx| val),
        Expr::String(_string) => Err("Strings not supported")?,
        Expr::Var(var_name) => {
            let field_getter =
                Ctx::field_getter(var_name).ok_or(format!("Unknown variable ${var_name}"))?;
            Box::new(field_getter)
        }
        Expr::UnOp(op, rhs) => {
            let rhs = compile_expr(rhs)?;
            match op {
                UnOp::Neg => Box::new(move |ctx| -rhs(ctx)),
                UnOp::Plus => rhs,
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let lhs = compile_expr(lhs)?;
            let rhs = compile_expr(rhs)?;
            match op {
                BinOp::Add => Box::new(move |ctx| lhs(ctx) + rhs(ctx)),
                BinOp::Sub => Box::new(move |ctx| lhs(ctx) - rhs(ctx)),
                BinOp::Mul => Box::new(move |ctx| lhs(ctx) * rhs(ctx)),
                BinOp::Div => Box::new(move |ctx| lhs(ctx) / rhs(ctx)),
            }
        }
        Expr::FieldAccess(_object, _field_name) => Err("Field access not supported")?,
        Expr::FuncCall(_function, _arguments) => Err("Function calls not supported")?,
    })
}

pub fn eval<Ctx: ExprContext>(input: &str, ctx: &Ctx) -> Result<f64, String> {
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?
    }
    let compiled_expr = compile_expr(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}

#[cfg(test)]
mod tests {
    use crate::{ExprContext, eval};

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
        assert_eq!(eval("2 * (foo + bar)", &ctx), Ok(2.0 * (ctx.foo + ctx.bar)));
    }
}
