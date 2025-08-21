use crate::{
    boxed_fn::DynBoxedFn,
    compiler::CompilerInner,
    expr::{BinOp, UnOp},
};

pub trait SupportedType: Sized + 'static {
    // register type with compiler, call all these register_bin_op, register_field
    fn register<Arg: SupportedType>(compiler: &mut CompilerInner<Arg>);

    // create a DynBoxedFn that takes Arg of type Obj and returns a field
    // function always take a function that returns a reference to a field
    // but can return DynBoxedFn that returns either reference or a value
    // for primitive types it should be value, as operators and casts only work with values
    // for objects it should be reference
    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn;
}

pub fn register_basic_types<Arg: SupportedType + 'static>(compiler: &mut CompilerInner<Arg>) {
    compiler.register_type::<i64>();
    compiler.register_type::<f64>();
}

impl SupportedType for () {
    fn register<Arg: SupportedType>(_compiler: &mut CompilerInner<Arg>) {}

    fn make_getter<Obj: 'static>(_getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |_obj: &Obj| ())
    }
}

impl SupportedType for i64 {
    fn register<Arg: SupportedType>(compiler: &mut CompilerInner<Arg>) {
        compiler
            .register_cast(|i: i64| i as f64)
            .register_un_op(UnOp::Neg, |i: i64| -i)
            .register_un_op(UnOp::Plus, |i: i64| i)
            .register_bin_op(BinOp::Add, |a: i64, b| a + b)
            .register_bin_op(BinOp::Sub, |a: i64, b| a - b)
            .register_bin_op(BinOp::Mul, |a: i64, b| a * b)
            .register_bin_op(BinOp::Div, |a: i64, b| a / b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |obj: &Obj| *getter(obj))
    }
}

impl SupportedType for f64 {
    fn register<Arg: SupportedType>(compiler: &mut CompilerInner<Arg>) {
        compiler
            .register_un_op(UnOp::Neg, |i: f64| -i)
            .register_un_op(UnOp::Plus, |i: f64| i)
            .register_bin_op(BinOp::Add, |a: f64, b| a + b)
            .register_bin_op(BinOp::Sub, |a: f64, b| a - b)
            .register_bin_op(BinOp::Mul, |a: f64, b| a * b)
            .register_bin_op(BinOp::Div, |a: f64, b| a / b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |obj: &Obj| *getter(obj))
    }
}
