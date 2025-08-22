use crate::{BinOp, BoxedFnRetVal, CompilerInner, CompilerResult, DynBoxedFn, UnOp};

pub trait SupportedType: Sized + 'static {
    // register type with compiler, call all these register_bin_op, register_field
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>);

    // create a DynBoxedFn that takes Arg of type Obj and returns a field
    // function always take a function that returns a reference to a field
    // but can return DynBoxedFn that returns either reference or a value
    // for primitive types it should be value, as operators and casts only work with values
    // for objects it should be reference
    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn;
}

pub fn register_basic_types<Ctx: SupportedType + 'static>(compiler: &mut CompilerInner<Ctx>) {
    compiler.register_type::<i64>();
    compiler.register_type::<f64>();
    compiler.register_type::<String>();
}

impl SupportedType for () {
    fn register<Ctx: SupportedType>(_compiler: &mut CompilerInner<Ctx>) {}

    fn make_getter<Obj: 'static>(_getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |_obj: &Obj| ())
    }
}

impl SupportedType for i64 {
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler
            .register_cast(|i: i64| format!("{i}"))
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
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler
            .register_cast(|f: f64| format!("{f}"))
            .register_un_op(UnOp::Neg, |f: f64| -f)
            .register_un_op(UnOp::Plus, |f: f64| f)
            .register_bin_op(BinOp::Add, |a: f64, b| a + b)
            .register_bin_op(BinOp::Sub, |a: f64, b| a - b)
            .register_bin_op(BinOp::Mul, |a: f64, b| a * b)
            .register_bin_op(BinOp::Div, |a: f64, b| a / b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |obj: &Obj| *getter(obj))
    }
}

impl SupportedType for String {
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler.register_bin_op(BinOp::Add, |a: String, b| a + &b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_val(move |obj: &Obj| getter(obj).clone())
    }
}

impl<R> SupportedType for Box<dyn Fn() -> R>
where
    R: 'static,
{
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler.register_function(
            |_compiler, args: Vec<DynBoxedFn>| {
                if !args.is_empty() {
                    return Err(format!("Expected 0 arguments, got {}", args.len()));
                }
                Ok(DynBoxedFn::make_ret_val(|_: &Ctx| ()))
            },
            |func: &Self, _arg: ()| (func)(),
        );
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_ref(move |obj: &Obj| getter(obj))
    }
}

impl<A, R> SupportedType for Box<dyn Fn(A) -> R>
where
    A: 'static,
    R: 'static,
{
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler.register_function(
            |compiler, mut args: Vec<DynBoxedFn>| {
                if args.len() != 1 {
                    return Err(format!("Expected 1 arguments, got {}", args.len()));
                }
                let arg = compiler
                    .cast::<A>(args.pop().unwrap())?
                    .get_fn_ret_val::<Ctx, A>()?;
                Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| arg.call(ctx)))
            },
            |func: &Self, arg: A| (func)(arg),
        );
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_ref(move |obj: &Obj| getter(obj))
    }
}

impl<A0, A1, R> SupportedType for Box<dyn Fn(A0, A1) -> R>
where
    A0: 'static,
    A1: 'static,
    R: 'static,
{
    fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
        compiler.register_function(
            |compiler, mut args: Vec<DynBoxedFn>| {
                if args.len() != 2 {
                    return Err(format!("Expected 2 arguments, got {}", args.len()));
                }

                let arg1 = compiler
                    .cast::<A1>(args.pop().unwrap())?
                    .get_fn_ret_val::<Ctx, A1>()?;
                let arg0 = compiler
                    .cast::<A0>(args.pop().unwrap())?
                    .get_fn_ret_val::<Ctx, A0>()?;

                Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                    (arg0.call(ctx), arg1.call(ctx))
                }))
            },
            |func: &Self, (a0, a1): (A0, A1)| (func)(a0, a1),
        );
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
        DynBoxedFn::make_ret_ref(move |obj: &Obj| getter(obj))
    }
}
