use super::try_insert;
use crate::{DynFn, Error, EvalType, MethodCallData, RegistryAccess, Result};

macro_rules! register_method_call {
    ($name:ident, $count:tt $(, $arg:ident : $idx:tt )* ) => {
        pub fn $name<$($arg,)* Ret>(
            &mut self,
            method: &'static str,
            method_fn: for<'a> fn(&'a T $(, $arg::RefType<'a> )* ) -> Ret::RefType<'a>,
        ) -> Result<()>
        where
            $($arg: EvalType,)*
            Ret: EvalType,
        {
            $(
                self.register_type::<$arg>()?;
            )*
            self.register_type::<Ret>()?;

            let compile_fn = Box::new(
                #[allow(unused_mut, non_snake_case)]
                move |object: DynFn, mut args: Vec<DynFn>| -> Result<DynFn> {
                    if args.len() != $count {
                        return Err(Error::InternalArgCountMismatch { expected: $count, got: args.len() });
                    }

                    $(
                        let $arg = args[$idx]
                            .downcast::<Ctx, $arg>()?;
                    )*

                    let object = object
                        .downcast::<Ctx, T>()?;

                    Ok(Ret::make_dyn_fn(move |ctx: &Ctx| {
                        method_fn(object(ctx) $(, $arg(ctx) )* )
                    }))
                },
            );

            let ty = T::type_info();
            let arg_types = vec![$($arg::type_info(),)*];

            let data = MethodCallData {
                compile_fn,
                arg_types,
            };

            try_insert(
                &mut self.registry.method_calls,
                (ty, method),
                data,
                || Error::DuplicateMethod { ty, method }
            )
        }
    };
}

impl<'r, Ctx, T> RegistryAccess<'r, Ctx, T>
where
    Ctx: EvalType,
    T: for<'a> EvalType<RefType<'a> = &'a T>,
{
    register_method_call!(register_method_call_0, 0);
    register_method_call!(register_method_call_1, 1, A1: 0);
    register_method_call!(register_method_call_2, 2, A1: 0, A2: 1);
    register_method_call!(register_method_call_3, 3, A1: 0, A2: 1, A3: 2);
    register_method_call!(register_method_call_4, 4, A1: 0, A2: 1, A3: 2, A4: 3);
    register_method_call!(register_method_call_5, 5, A1: 0, A2: 1, A3: 2, A4: 3, A5: 4);
}
