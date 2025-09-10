use super::try_insert;
use crate::{DynFn, RegistryAccess, SupportedType};
use std::any::TypeId;

impl<'r, Ctx, T> RegistryAccess<'r, Ctx, T>
where
    Ctx: SupportedType,
    T: for<'a> SupportedType<RefType<'a> = &'a T>,
{
    pub fn register_method_call_0<Ret: SupportedType>(
        &mut self,
        method_name: &'static str,
        method_fn: for<'a> fn(&'a T) -> Ret::RefType<'a>,
    ) -> Result<(), String> {
        let key = (TypeId::of::<T>(), method_name, vec![]);
        let compile_func = Box::new(
            move |object: DynFn, args: Vec<DynFn>| -> Result<DynFn, String> {
                if !args.is_empty() {
                    return Err(format!(
                        "Expected 0 arguments, got {}",
                        args.len()
                    ));
                }
                let object = object
                    .downcast::<Ctx, T>()
                    .ok_or("Failed to downcast object".to_string())?;

                Ok(Ret::make_dyn_fn(move |ctx: &Ctx| method_fn(object(ctx))))
            },
        );
        try_insert(&mut self.registry.method_calls, key, compile_func)
    }
}
