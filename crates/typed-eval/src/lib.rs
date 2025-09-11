#![cfg_attr(feature = "nightly", feature(min_specialization))]

// helps derived macro refer to this crate by name, from within the same crate
extern crate self as typed_eval;

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

pub use typed_eval_macro::{SupportedType, supported_type_methods};

#[cfg(feature = "nightly")]
#[rustversion::not(nightly)]
compile_error!("The `nightly` feature requires a nightly compiler");

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

    #[derive(Debug, PartialEq, SupportedType)]
    struct User {
        // name: String,
        age: i64,
    }

    #[supported_type_methods]
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

        fn return_nothing(&self) {}
    }

    #[derive(SupportedType)]
    struct TestContext {
        foo: i64,
        bar: f64,
        user: User,
        user_b: User,
    }

    #[supported_type_methods]
    impl TestContext {
        fn get_user(&self, n: i64) -> &User {
            match n % 2 {
                0 => &self.user,
                _ => &self.user_b,
            }
        }
    }

    #[test]
    fn test_eval() {
        let ctx = TestContext {
            foo: 1,
            bar: 2.5,
            user: User {
                // name: "John Doe".to_string(),
                age: 45,
            },
            user_b: User {
                // name: "Alice".to_string(),
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

        assert_eq!(eval::<_, User>("get_user(0)", &ctx), Ok(&ctx.user));
        assert_eq!(eval::<_, User>("get_user(1)", &ctx), Ok(&ctx.user_b));

        assert_eq!(eval::<_, ()>("user.return_nothing()", &ctx), Ok(()));

        assert_eq!(eval::<_, i64>("get_user(0).age", &ctx), Ok(45));
    }
}
