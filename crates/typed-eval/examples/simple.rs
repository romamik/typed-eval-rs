use std::borrow::Cow;
use typed_eval::{Compiler, EvalType, eval_type_methods};

#[derive(EvalType)]
struct User {
    name: String,
    age: i64,
}

#[eval_type_methods]
impl User {
    fn greet(&self) -> Cow<'_, str> {
        format!("Hello, {}", self.name).into()
    }
}

#[derive(EvalType)]
#[typed_eval(no_methods)]
struct Context {
    user: User,
}

fn main() {
    let compiler = Compiler::new().unwrap();

    let greet_user = compiler.compile::<String>("user.greet()").unwrap();
    let double_age = compiler.compile::<i64>("user.age * 2").unwrap();

    let context = Context {
        user: User {
            name: "Bob".into(),
            age: 45,
        },
    };

    assert_eq!(greet_user(&context), "Hello, Bob");
    assert_eq!(double_age(&context), 90);
}
