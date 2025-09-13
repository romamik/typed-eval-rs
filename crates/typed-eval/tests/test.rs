#![cfg_attr(feature = "nightly", feature(min_specialization))]

mod common;
use common::*;

#[test]
fn test_math_expressions() {
    let ctx = make_context();

    // Simple math without context
    assert_eq!(eval::<_, f64>("(1 + 2) * 3", &ctx), Ok(9.0));
    assert_eq!(
        eval::<_, f64>("2 * (foo + bar)", &ctx),
        Ok(2.0 * (ctx.foo as f64 + ctx.bar))
    );
}

#[test]
fn test_context_field_access() {
    let ctx = make_context();

    // Direct fields from context
    assert_eq!(eval::<_, i64>("foo", &ctx), Ok(1));
    assert_eq!(eval::<_, f64>("bar", &ctx), Ok(2.5));

    // Nested field access
    assert_eq!(eval::<_, i64>("get_user(0).age", &ctx), Ok(45));
}

#[test]
fn test_struct_fields() {
    let ctx = make_context();

    // Ignored field should error
    assert!(eval::<_, i64>("some_struct.ignored_field", &ctx).is_err());

    // Renamed field access
    assert!(eval::<_, i64>("some_struct.old_field_name", &ctx).is_err());
    assert_eq!(eval::<_, i64>("some_struct.new_field_name", &ctx), Ok(10));
}

#[test]
fn test_methods() {
    let ctx = make_context();

    // Methods on User
    assert_eq!(eval::<_, f64>("user.get_age()", &ctx), Ok(45.0));
    assert_eq!(eval::<_, i64>("user.get_age_multiplied(2)", &ctx), Ok(90));
    assert_eq!(eval::<_, i64>("user.get_age_clamped(40, 50)", &ctx), Ok(45));
    assert_eq!(eval::<_, i64>("user.get_age_clamped(50, 60)", &ctx), Ok(50));
    assert_eq!(eval::<_, i64>("user.get_age_clamped(30, 40)", &ctx), Ok(40));
    assert_eq!(eval::<_, i64>("user.age_diff(user_b)", &ctx), Ok(5));
    assert_eq!(eval::<_, ()>("user.return_nothing()", &ctx), Ok(()));

    // Methods on TestContext
    assert_eq!(eval::<_, User>("get_user(0)", &ctx), Ok(&ctx.user));
    assert_eq!(eval::<_, User>("get_user(1)", &ctx), Ok(&ctx.user_b));
}
