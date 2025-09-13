use typed_eval::{EvalType, eval_type_methods};

#[derive(EvalType)]
#[typed_eval(no_methods)]
pub struct SomeStruct {
    #[typed_eval(ignore)]
    #[allow(unused)]
    pub ignored_field: String,

    #[typed_eval(rename = "new_field_name")]
    pub old_field_name: i64,
}

#[derive(Debug, PartialEq, EvalType)]
pub struct User {
    pub age: i64,
}

#[eval_type_methods]
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

#[derive(EvalType)]
pub struct TestContext {
    pub foo: i64,
    pub bar: f64,
    pub user: User,
    pub user_b: User,
    pub some_struct: SomeStruct,
}

#[eval_type_methods]
impl TestContext {
    fn get_user(&self, n: i64) -> &User {
        match n % 2 {
            0 => &self.user,
            _ => &self.user_b,
        }
    }
}

pub fn make_context() -> TestContext {
    TestContext {
        foo: 1,
        bar: 2.5,
        user: User { age: 45 },
        user_b: User { age: 40 },
        some_struct: SomeStruct {
            ignored_field: "Hello".into(),
            old_field_name: 10,
        },
    }
}
