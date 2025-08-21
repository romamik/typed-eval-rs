use crate::{
    boxed_fn::DynBoxedFn, compiler::Compiler, expr_parser::parse_expr,
    supported_type::SupportedType,
};

pub mod boxed_fn;
pub mod compiler;
pub mod expr;
pub mod expr_parser;
pub mod supported_type;
pub mod tdesc;

struct Foo {
    a: i64,
    b: f64,
}

impl SupportedType for Foo {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_field("a", |obj: &Self| &obj.a)
            .register_field("b", |obj: &Self| &obj.b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_ref(getter)
    }
}

struct Bar {
    c: i64,
    foo: Foo,
}

impl SupportedType for Bar {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_field("c", |obj: &Self| &obj.c)
            .register_field("foo", |obj: &Self| &obj.foo);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_ref(getter)
    }
}

fn main() {
    let src = "  (foo.a*foo.b - c) * +-+-2.0 / --2 ";
    let parsed = parse_expr(src).unwrap();
    let compiler = Compiler::<Bar>::new();
    let compiled = compiler.compile::<f64>(&parsed).unwrap();
    let ctx = Bar {
        foo: Foo { a: 10, b: 20.5 },
        c: 20,
    };
    let result = compiled.call(&ctx);
    println!("{result:?}");
}
