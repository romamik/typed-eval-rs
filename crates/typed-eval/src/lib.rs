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

pub fn eval<Ctx: for<'a> SupportedType<RefType<'a> = &'a Ctx>>(
    input: &str,
    ctx: &Ctx,
) -> Result<f64, String> {
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?;
    }

    let compiler = Compiler::<Ctx>::new().unwrap();
    let compiled_expr = compiler.compile::<f64>(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}

#[cfg(test)]
mod tests {
    use crate::*;

    struct User {
        name: String,
        age: i64,
    }

    impl SupportedType for User {
        type RefType<'a> = &'a User;
        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            // registry.register_field_access::<Self, String>(
            //     "name",
            //     |_: &Ctx, obj: &User| obj.name.clone(),
            // )?;
            registry.register_field_access::<Self, i64>(
                "age",
                |_: &Ctx, obj: &User| obj.age.clone(),
            )?;
            Ok(())
        }
    }

    struct TestContext {
        foo: i64,
        bar: f64,
        user: User,
    }

    impl SupportedType for TestContext {
        type RefType<'a> = &'a TestContext;

        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            registry.register_field_access::<Self, i64>(
                "foo",
                |_: &Ctx, obj: &TestContext| obj.foo,
            )?;
            registry.register_field_access::<Self, f64>(
                "bar",
                |_: &Ctx, obj: &TestContext| obj.bar,
            )?;
            registry.register_field_access::<Self, User>(
                "user",
                |_: &Ctx, obj: &TestContext| &obj.user,
            )?;

            registry.register_type::<i64>()?;
            registry.register_type::<f64>()?;
            registry.register_type::<User>()?;

            Ok(())
        }
    }

    #[test]
    fn test_eval() {
        let ctx = TestContext {
            foo: 1,
            bar: 2.5,
            user: User {
                name: "John Doe".to_string(),
                age: 45,
            },
        };

        assert_eq!(eval("(1 + 2) * 3", &ctx), Ok((1.0 + 2.0) * 3.0));
        assert_eq!(
            eval("2 * (foo + bar)", &ctx),
            Ok(2.0 * (ctx.foo as f64 + ctx.bar))
        );

        assert_eq!(eval("0.5 * user.age", &ctx), Ok(0.5 * ctx.user.age as f64));
    }
}
