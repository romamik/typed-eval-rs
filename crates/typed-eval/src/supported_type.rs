use crate::{BinOp, CompilerInner, DynBoxedFn, UnOp};

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

macro_rules! impl_supported_type_for_fn {
    ($($args:ident),*) => {
        impl<$($args,)* R> SupportedType for Box<dyn Fn($($args),*) -> R>
        where
            $($args: 'static,)*
            R: 'static,
        {
            #[allow(non_snake_case, unused_parens, unused_variables, unused_mut, clippy::unused_unit)]
            fn register<Ctx: SupportedType>(compiler: &mut CompilerInner<Ctx>) {
                compiler.register_function(
                    |compiler, mut args: Vec<DynBoxedFn>| {
                        let expected_arg_count = (&[ $(stringify!($args)),* ] as &[&str]).len();

                        if args.len() != expected_arg_count {
                            return Err(format!("Expected {} arguments, got {}", expected_arg_count, args.len()));
                        }

                        $(
                            let $args = compiler
                                .cast::<$args>(args.remove(0))?
                                .get_fn_ret_val::<Ctx, $args>()?;
                        )*

                        Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                            ( $( $args.call(ctx) ),* )
                        }))
                    },
                    |func: &Self, ( $($args),* ): ( $($args),* )| (func)( $($args),* ),
                );
            }

            fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + Clone + 'static) -> DynBoxedFn {
                DynBoxedFn::make_ret_ref(move |obj: &Obj| getter(obj))
            }
        }

    };
}

impl_supported_type_for_fn!();
impl_supported_type_for_fn!(A);
impl_supported_type_for_fn!(A0, A1);
impl_supported_type_for_fn!(A0, A1, A2);
impl_supported_type_for_fn!(A0, A1, A2, A3);
impl_supported_type_for_fn!(A0, A1, A2, A3, A4);
