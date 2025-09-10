mod compiler;
mod compiler_registry;
mod dyn_fn;
mod expr;
mod expr_parser;
mod method;
mod supported_type;

pub use compiler::*;
pub use compiler_registry::*;
pub use dyn_fn::*;
pub use expr::*;
pub use expr_parser::*;
pub use method::*;
pub use supported_type::*;

pub fn eval<'a, Ctx, Ret>(
    input: &str,
    ctx: &'a Ctx,
) -> Result<Ret::RefType<'a>, String>
where
    Ctx: ExprContext,
    Ret: SupportedType,
{
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
    let compiled_expr = compiler.compile::<Ret>(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}

#[cfg(test)]
mod tests {

    use crate::*;

    #[derive(Debug, PartialEq)]
    struct User {
        name: String,
        age: i64,
    }

    impl User {
        fn get_age(&self) -> i64 {
            self.age
        }

        fn get_age_multiplied(&self, factor: i64) -> i64 {
            self.age * factor
        }

        fn get_age_clamped(&self, min: i64, max: i64) -> i64 {
            self.age.clamp(min, max)
        }

        fn age_diff(&self, other: &User) -> i64 {
            self.age - other.age
        }
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
            registry.register_field_access::<i64>(
                "age",
                |_: &Ctx, obj: &User| obj.age,
            )?;

            registry.register_method_call_0::<i64>("get_age", Self::get_age)?;

            registry.register_method_call::<i64, i64>(
                "get_age_multiplied",
                CompileMethod1Args(|_, user: &Self, factor: i64| {
                    user.get_age_multiplied(factor)
                }),
            )?;

            registry.register_method_call::<(i64, i64), i64>(
                "get_age_clamped",
                CompileMethod2Args(|_, user: &Self, min: i64, max: i64| {
                    user.get_age_clamped(min, max)
                }),
            )?;

            registry.register_method_call::<User, i64>(
                "age_diff",
                CompileMethod1Args(|_, user: &Self, other: &User| {
                    user.age_diff(other)
                }),
            )?;

            Ok(())
        }
    }

    struct TestContext {
        foo: i64,
        bar: f64,
        user: User,
        userB: User,
    }

    impl SupportedType for TestContext {
        type RefType<'a> = &'a TestContext;

        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            registry.register_field_access::<i64>(
                "foo",
                |_: &Ctx, obj: &TestContext| obj.foo,
            )?;
            registry.register_field_access::<f64>(
                "bar",
                |_: &Ctx, obj: &TestContext| obj.bar,
            )?;
            registry.register_field_access::<User>(
                "user",
                |_: &Ctx, obj: &TestContext| &obj.user,
            )?;
            registry.register_field_access::<User>(
                "userB",
                |_: &Ctx, obj: &TestContext| &obj.userB,
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
            userB: User {
                name: "Alice".to_string(),
                age: 40,
            },
        };

        assert_eq!(eval::<_, f64>("(1 + 2) * 3", &ctx), Ok((1.0 + 2.0) * 3.0));
        assert_eq!(
            eval::<_, f64>("2 * (foo + bar)", &ctx),
            Ok(2.0 * (ctx.foo as f64 + ctx.bar))
        );

        assert_eq!(
            eval::<_, f64>("0.5 * user.age", &ctx),
            Ok(0.5 * ctx.user.age as f64)
        );

        let user: &User = eval::<_, User>("user", &ctx).unwrap();
        assert_eq!(user, &ctx.user);

        assert_eq!(eval::<_, f64>("user.get_age()", &ctx), Ok(45.0));
        assert_eq!(eval::<_, i64>("user.get_age_multiplied(2)", &ctx), Ok(90));
        assert_eq!(
            eval::<_, i64>("user.get_age_clamped(40, 50)", &ctx),
            Ok(45)
        );
        assert_eq!(
            eval::<_, i64>("user.get_age_clamped(50, 60)", &ctx),
            Ok(50)
        );
        assert_eq!(
            eval::<_, i64>("user.get_age_clamped(30, 40)", &ctx),
            Ok(40)
        );
        assert_eq!(eval::<_, i64>("user.age_diff(userB)", &ctx), Ok(5));
    }
}
