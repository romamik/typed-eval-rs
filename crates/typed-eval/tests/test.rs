#![cfg_attr(feature = "nightly", feature(min_specialization))]

mod common;

use common::*;
use std::borrow::Cow;
use typed_eval::{Error, EvalType, eval_type_methods};

#[test]
fn test_math_operations() {
    let ctx = ();

    assert_eq!(eval::<_, i64>("1 + 2 * 3", &ctx), Ok(7));
    assert_eq!(eval::<_, f64>("1.5 + 2.5 * 2.0", &ctx), Ok(6.5));
    assert_eq!(eval::<_, f64>("(1.0 + 2.0) * 3.0", &ctx), Ok(9.0));
    assert_eq!(eval::<_, i64>("10 / 3", &ctx), Ok(3));
    assert_eq!(eval::<_, f64>("10.0 / 4.0", &ctx), Ok(2.5));
}

#[test]
fn test_context_fields() {
    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct Ctx {
        foo: i64,
        bar: f64,
    }

    let ctx = Ctx { foo: 42, bar: 2.5 };

    assert_eq!(eval::<_, i64>("foo", &ctx), Ok(42));
    assert_eq!(eval::<_, f64>("bar", &ctx), Ok(2.5));
    assert_eq!(eval::<_, f64>("foo + bar", &ctx), Ok(44.5));
}

#[test]
fn test_field_on_function_result() {
    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct User {
        age: i64,
    }

    #[derive(EvalType)]
    struct Ctx {
        user_a: User,
        user_b: User,
    }

    #[eval_type_methods]
    impl Ctx {
        fn get_user(&self, n: i64) -> &User {
            if n % 2 == 0 {
                &self.user_a
            } else {
                &self.user_b
            }
        }
    }

    let ctx = Ctx {
        user_a: User { age: 50 },
        user_b: User { age: 40 },
    };

    assert_eq!(eval::<_, i64>("get_user(0).age", &ctx), Ok(50));
    assert_eq!(eval::<_, i64>("get_user(1).age", &ctx), Ok(40));
}

#[test]
fn test_struct_field_attributes() {
    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct S {
        #[typed_eval(ignore)]
        #[allow(unused)]
        ignored: i64,
        #[typed_eval(rename = "renamed")]
        original: i64,
    }

    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct Ctx {
        s: S,
    }

    let ctx = Ctx {
        s: S {
            ignored: 1,
            original: 2,
        },
    };

    assert_eq!(
        eval::<_, i64>("s.ignored", &ctx),
        Err(Error::FieldNotFound {
            ty: S::type_info(),
            field: "ignored".into()
        }
        .into())
    );
    assert_eq!(
        eval::<_, i64>("s.original", &ctx),
        Err(Error::FieldNotFound {
            ty: S::type_info(),
            field: "original".into()
        }
        .into())
    );
    assert_eq!(eval::<_, i64>("s.renamed", &ctx), Ok(2));
}

#[test]
fn test_methods() {
    #[derive(Debug, PartialEq, EvalType)]
    struct User {
        age: i64,
    }

    #[eval_type_methods]
    impl User {
        fn double_age(&self) -> i64 {
            self.age * 2
        }

        fn add_to_age(&self, x: i64) -> i64 {
            self.age + x
        }

        fn clamp_age(&self, min: i64, max: i64) -> i64 {
            self.age.clamp(min, max)
        }
    }

    #[derive(EvalType)]
    struct Ctx {
        user: User,
        user_b: User,
    }

    #[eval_type_methods]
    impl Ctx {
        fn get_user(&self, n: i64) -> &User {
            if n % 2 == 0 { &self.user } else { &self.user_b }
        }
    }

    let ctx = Ctx {
        user: User { age: 10 },
        user_b: User { age: 20 },
    };

    assert_eq!(eval::<_, i64>("user.double_age()", &ctx), Ok(20));
    assert_eq!(eval::<_, i64>("user.add_to_age(5)", &ctx), Ok(15));
    assert_eq!(eval::<_, i64>("user.clamp_age(5, 12)", &ctx), Ok(10));
    assert_eq!(eval::<_, i64>("user.clamp_age(15, 30)", &ctx), Ok(15));

    assert_eq!(eval::<_, i64>("get_user(0).double_age()", &ctx), Ok(20));
    assert_eq!(eval::<_, i64>("get_user(0).add_to_age(7)", &ctx), Ok(17));
    assert_eq!(
        eval::<_, i64>("get_user(1).clamp_age(15, 30)", &ctx),
        Ok(20)
    );
}

#[cfg(feature = "nightly")]
#[test]
fn test_nightly_skip_no_methods() {
    #[derive(EvalType)]
    struct S {
        value: i64,
    }

    let s = S { value: 5 };

    assert_eq!(eval::<_, i64>("value", &s), Ok(5));
}

#[test]
fn test_string_literals() {
    let ctx = ();

    assert_eq!(eval::<_, String>("\"hello\"", &ctx), Ok("hello".into()));
    assert_eq!(eval::<_, String>("\"world\"", &ctx), Ok("world".into()));
}

#[test]
fn test_string_concatenation() {
    let ctx = ();

    assert_eq!(
        eval::<_, String>("\"hello\" + \" world\"", &ctx),
        Ok("hello world".into())
    );
    assert_eq!(
        eval::<_, String>("\"foo\" + \"bar\"", &ctx),
        Ok("foobar".into())
    );
}

#[test]
fn test_string_context_fields() {
    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct Ctx {
        greeting: String,
        name: String,
    }

    let ctx = Ctx {
        greeting: "Hello".to_string(),
        name: "Alice".to_string(),
    };

    assert_eq!(eval::<_, String>("greeting", &ctx), Ok("Hello".into()));
    assert_eq!(eval::<_, String>("name", &ctx), Ok("Alice".into()));
    assert_eq!(
        eval::<_, String>("greeting + \", \" + name", &ctx),
        Ok("Hello, Alice".into())
    );
}

#[test]
fn test_mixed_string_with_casts() {
    #[derive(EvalType)]
    #[typed_eval(no_methods)]
    struct Ctx {
        num: i64,
        pi: f64,
    }

    #[allow(clippy::approx_constant)]
    let ctx = Ctx { num: 42, pi: 3.14 };

    assert_eq!(
        eval::<_, String>("\"Number: \" + num", &ctx),
        Ok("Number: 42".into())
    );
    assert_eq!(
        eval::<_, String>("\"Pi is approximately \" + pi", &ctx),
        Ok("Pi is approximately 3.14".into())
    );
}

#[test]
fn test_method_return_cow() {
    #[derive(EvalType)]
    struct Ctx {
        str: String,
    }

    #[eval_type_methods]
    impl Ctx {
        fn return_cow(&self) -> Cow<'_, str> {
            self.str.as_str().into()
        }
    }

    assert_eq!(
        eval::<_, String>("return_cow()", &Ctx { str: "Hi!".into() }).unwrap(),
        "Hi!"
    );
}
