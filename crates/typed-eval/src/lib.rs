#![cfg_attr(feature = "nightly", feature(min_specialization))]

#[cfg(feature = "nightly")]
#[rustversion::not(nightly)]
compile_error!("The `nightly` feature requires a nightly compiler");

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

pub fn eval<'a, Ctx, Ret>(
    input: &str,
    ctx: &'a Ctx,
) -> Result<Ret::RefType<'a>, String>
where
    Ctx: SupportedType,
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

    impl SupportedTypeMethods for User {
        fn register_methods<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            registry.register_method_call_0::<i64>("get_age", Self::get_age)?;

            registry.register_method_call_1::<i64, i64>(
                "get_age_multiplied",
                Self::get_age_multiplied,
            )?;

            registry.register_method_call_2::<i64, i64, i64>(
                "get_age_clamped",
                Self::get_age_clamped,
            )?;

            registry.register_method_call_1::<User, i64>(
                "age_diff",
                Self::age_diff,
            )?;
            Ok(())
        }
    }

    impl SupportedType for User {
        type RefType<'a> = &'a User;

        fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
            self
        }

        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            // registry.register_field_access::<Self, String>(
            //     "name",
            //     |_: &Ctx, obj: &User| obj.name.clone(),
            // )?;
            registry.register_field_access::<i64>("age", |obj: &User| {
                obj.age.to_ref_type()
            })?;

            Ok(())
        }
    }

    struct TestContext {
        foo: i64,
        bar: f64,
        user: User,
        user_b: User,
    }

    impl SupportedTypeMethods for TestContext {
        fn register_methods<Ctx: SupportedType>(
            _registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            Ok(())
        }
    }

    impl SupportedType for TestContext {
        type RefType<'a> = &'a TestContext;

        fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
            self
        }

        fn register<Ctx: SupportedType>(
            mut registry: RegistryAccess<Ctx, Self>,
        ) -> Result<(), String> {
            registry.register_field_access::<i64>("foo", |obj| {
                obj.foo.to_ref_type()
            })?;
            registry.register_field_access::<f64>("bar", |obj| {
                obj.bar.to_ref_type()
            })?;
            registry.register_field_access::<User>("user", |obj| {
                obj.user.to_ref_type()
            })?;
            registry.register_field_access::<User>("user_b", |obj| {
                obj.user_b.to_ref_type()
            })?;

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
            user_b: User {
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
        assert_eq!(eval::<_, i64>("user.age_diff(user_b)", &ctx), Ok(5));
    }
}
