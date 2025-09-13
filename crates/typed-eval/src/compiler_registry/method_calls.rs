use super::try_insert;
use crate::{
    DynFn, EvalType, RegistryAccess, compiler_registry::MethodCallData,
};
use std::any::TypeId;

macro_rules! register_method_call {
    ($name:ident, $count:tt $(, $arg:ident : $idx:tt )* ) => {
        pub fn $name<$($arg,)* Ret>(
            &mut self,
            method_name: &'static str,
            method_fn: for<'a> fn(&'a T $(, $arg::RefType<'a> )* ) -> Ret::RefType<'a>,
        ) -> Result<(), String>
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
                move |object: DynFn, mut args: Vec<DynFn>| -> Result<DynFn, String> {
                    if args.len() != $count {
                        return Err(format!(
                            "Expected {} arguments, got {}",
                            $count,
                            args.len()
                        ));
                    }

                    $(
                        let $arg = args[$idx]
                            .downcast::<Ctx, $arg>()
                            .ok_or(format!("Failed to downcast argument {}", $idx + 1))?;
                    )*

                    let object = object
                        .downcast::<Ctx, T>()
                        .ok_or("Failed to downcast object".to_string())?;

                    Ok(Ret::make_dyn_fn(move |ctx: &Ctx| {
                        method_fn(object(ctx) $(, $arg(ctx) )* )
                    }))
                },
            );

            let key = (TypeId::of::<T>(), method_name);
            let arg_types = vec![$(TypeId::of::<$arg>(),)*];

            let data = MethodCallData {
                compile_fn,
                arg_types,
            };

            try_insert(&mut self.registry.method_calls, key, data)
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
