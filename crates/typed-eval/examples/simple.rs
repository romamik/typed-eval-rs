use typed_eval::{Compiler, SupportedType, parse_expr};

#[derive(SupportedType)]
struct User {
    name: String,
    age: i64,
}

#[derive(SupportedType)]
struct Context {
    user: User,
    greet: Box<dyn Fn(String) -> String>,
}

fn main() {
    let compiler = Compiler::new();

    let greet_user = compiler.compile::<String>("greet(user.name)").unwrap();
    let double_age = compiler.compile::<i64>("user.age * 2").unwrap();

    let context = Context {
        user: User {
            name: "Bob".into(),
            age: 45,
        },
        greet: Box::new(|name| format!("Hello, {name}")),
    };

    assert_eq!(greet_user.call(&context), "Hello, Bob");
    assert_eq!(double_age.call(&context), 90);
}
