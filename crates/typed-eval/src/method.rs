use crate::{DynFn, SupportedType};
use std::any::TypeId;

pub trait CompileMethod<Ctx, O, A, R>: 'static {
    fn get_arg_types(&self) -> Vec<TypeId>;
    fn compile(&self, object: DynFn, args: Vec<DynFn>)
    -> Result<DynFn, String>;
}

pub struct CompileMethod0Args<Ctx, Obj, Ret>(
    pub for<'a> fn(&'a Ctx, Obj::RefType<'a>) -> Ret::RefType<'a>,
)
where
    Obj: SupportedType,
    Ret: SupportedType;

impl<Ctx, O, R> CompileMethod<Ctx, O, (), R> for CompileMethod0Args<Ctx, O, R>
where
    Ctx: 'static,
    O: SupportedType,
    R: SupportedType,
{
    fn get_arg_types(&self) -> Vec<TypeId> {
        vec![]
    }

    fn compile(
        &self,
        object: DynFn,
        #[allow(unused_mut)] mut args: Vec<DynFn>,
    ) -> Result<DynFn, String> {
        let expected_arg_count = 0;
        if args.len() != expected_arg_count {
            return Err(format!(
                "Expected {expected_arg_count} arguments, got {}",
                args.len()
            ));
        }
        let object = object
            .downcast::<Ctx, O>()
            .ok_or("Failed to downcast object".to_string())?;
        let method = self.0;
        Ok(R::make_dyn_fn(move |ctx| (method)(ctx, object(ctx))))
    }
}

pub struct CompileMethod1Args<Ctx, Obj, Arg, Ret>(
    pub  for<'a> fn(
        &'a Ctx,
        Obj::RefType<'a>,
        Arg::RefType<'a>,
    ) -> Ret::RefType<'a>,
)
where
    Obj: SupportedType,
    Arg: SupportedType,
    Ret: SupportedType;

impl<Ctx, O, A, R> CompileMethod<Ctx, O, A, R>
    for CompileMethod1Args<Ctx, O, A, R>
where
    Ctx: 'static,
    O: SupportedType,
    A: SupportedType,
    R: SupportedType,
{
    fn get_arg_types(&self) -> Vec<TypeId> {
        vec![TypeId::of::<A>()]
    }

    fn compile(
        &self,
        object: DynFn,
        mut args: Vec<DynFn>,
    ) -> Result<DynFn, String> {
        let expected_arg_count = 1;
        if args.len() != expected_arg_count {
            return Err(format!(
                "Expected {expected_arg_count} arguments, got {}",
                args.len()
            ));
        }
        let arg = args
            .remove(0)
            .downcast::<Ctx, A>()
            .ok_or("Failed to downcast arg".to_string())?;
        let object = object
            .downcast::<Ctx, O>()
            .ok_or("Failed to downcast object".to_string())?;
        let method = self.0;
        Ok(R::make_dyn_fn(move |ctx| {
            (method)(ctx, object(ctx), arg(ctx))
        }))
    }
}

pub struct CompileMethod2Args<Ctx, Obj, Arg1, Arg2, Ret>(
    pub  for<'a> fn(
        &'a Ctx,
        Obj::RefType<'a>,
        Arg1::RefType<'a>,
        Arg2::RefType<'a>,
    ) -> Ret::RefType<'a>,
)
where
    Obj: SupportedType,
    Arg1: SupportedType,
    Arg2: SupportedType,
    Ret: SupportedType;

impl<Ctx, O, A1, A2, R> CompileMethod<Ctx, O, (A1, A2), R>
    for CompileMethod2Args<Ctx, O, A1, A2, R>
where
    Ctx: 'static,
    O: SupportedType,
    A1: SupportedType,
    A2: SupportedType,
    R: SupportedType,
{
    fn get_arg_types(&self) -> Vec<TypeId> {
        vec![TypeId::of::<A1>(), TypeId::of::<A2>()]
    }

    fn compile(
        &self,
        object: DynFn,
        mut args: Vec<DynFn>,
    ) -> Result<DynFn, String> {
        let expected_arg_count = 2;
        if args.len() != expected_arg_count {
            return Err(format!(
                "Expected {expected_arg_count} arguments, got {}",
                args.len()
            ));
        }
        let arg1 = args
            .remove(0)
            .downcast::<Ctx, A1>()
            .ok_or("Failed to downcast arg1".to_string())?;
        let arg2 = args
            .remove(0)
            .downcast::<Ctx, A2>()
            .ok_or("Failed to downcast arg2".to_string())?;
        let object = object
            .downcast::<Ctx, O>()
            .ok_or("Failed to downcast object".to_string())?;
        let method = self.0;
        Ok(R::make_dyn_fn(move |ctx| {
            (method)(ctx, object(ctx), arg1(ctx), arg2(ctx))
        }))
    }
}
