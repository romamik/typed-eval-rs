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
