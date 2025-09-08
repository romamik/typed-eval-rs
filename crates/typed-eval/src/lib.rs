mod compiler;
mod compiler_registry;
mod dyn_fn;
mod expr;
mod expr_parser;
mod supported_type;

pub use compiler::*;
pub use compiler_registry::*;
pub use dyn_fn::*;
pub use expr::*;
pub use expr_parser::*;
pub use supported_type::*;

pub fn eval<Ctx: SupportedType>(input: &str, ctx: &Ctx) -> Result<f64, String> {
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
    use std::rc::Rc;

    struct User {
        name: String,
        age: i64,
    }

    impl SupportedType for Rc<User> {
        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) {
            registry.register_field_access("name", |ctx: &Rc<User>| {
                ctx.name.clone()
            });
            registry
                .register_field_access("age", |ctx: &Rc<User>| ctx.age.clone());
        }
    }

    struct TestContext {
        foo: i64,
        bar: f64,
        user: Rc<User>,
    }

    impl SupportedType for Rc<TestContext> {
        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) {
            registry.register_field_access("foo", |ctx: &Rc<TestContext>| {
                ctx.foo.clone()
            });
            registry.register_field_access("bar", |ctx: &Rc<TestContext>| {
                ctx.bar.clone()
            });
            registry.register_field_access("user", |ctx: &Rc<TestContext>| {
                ctx.user.clone()
            });

            registry.register_type::<Rc<User>>();
        }
    }

    #[test]
    fn test_eval() {
        let ctx = Rc::new(TestContext {
            foo: 1,
            bar: 2.5,
            user: Rc::new(User {
                name: "John Doe".to_string(),
                age: 45,
            }),
        });

        assert_eq!(eval("(1 + 2) * 3", &ctx), Ok((1.0 + 2.0) * 3.0));
        assert_eq!(
            eval("2 * (foo + bar)", &ctx),
            Ok(2.0 * (ctx.foo as f64 + ctx.bar))
        );

        assert_eq!(eval("0.5 * user.age", &ctx), Ok(0.5 * ctx.user.age as f64));
    }
}
