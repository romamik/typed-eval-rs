# type-eval

**typed-eval** is a type-safe expression evaluation engine for Rust.
It lets you compile and execute dynamic expressions against a strongly typed context objects with full type checking at compile time.

## Features

* Compile and evaluate expression like `a + b * 10`
* Type-safe: every compiled expression has a known return type
* Field access (`user.age`) and function calls (`greet(user.name)`)
* Extensible - register types, casts, etc.

## Quick example

```rust
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
```

## How it works

**typed-eval** compiles expressions by combining closures into a single executable function.

For example, compiling the following expression: `"a + 10"`, generates three closures:
  * A closure to get `a` from the context:
  ```
  lhs = |ctx| ctx.a
  ```
  * A closure to return constant 10:
  ```
  rhs = |ctx| 10
  ```
  * A final closure that calls the first two, adds their results, and returns the sum.
  ```
  result = |ctx| lhs(ctx) + rhs(ctx)
  ```

This means the compiled function executes directly on the context with minimal overhead: no AST walking, no bytecode, etc.

## Status

This is an **experimental** project. 

## License

This project is licensed under the MIT License.
