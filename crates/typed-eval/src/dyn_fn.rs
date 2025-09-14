use crate::{Error, EvalType, Result, TypeInfo};
use std::{any::Any, ops::Deref};

// the function with a dynamically known type
pub struct DynFn {
    pub arg_type: TypeInfo,
    pub ret_type: TypeInfo,
    boxed_fun: Box<dyn ClonableAny>,
}

impl DynFn {
    pub fn new<Arg, Ret>(
        f: impl for<'a> Fn(&'a Arg) -> Ret::RefType<'a> + Clone + 'static,
    ) -> Self
    where
        Arg: EvalType,
        Ret: EvalType,
    {
        Self {
            boxed_fun: Box::new(BoxedFn(Box::new(f))),
            arg_type: Arg::type_info(),
            ret_type: Ret::type_info(),
        }
    }

    pub fn downcast<Arg, Ret>(&self) -> Result<BoxedFn<Arg, Ret>>
    where
        Arg: EvalType,
        Ret: EvalType,
    {
        self.boxed_fun
            .as_any()
            .downcast_ref()
            .cloned()
            .ok_or_else(|| Error::InternalDynFnDowncastError {
                expected_arg: Arg::type_info(),
                expected_ret: Ret::type_info(),
                got_arg: self.arg_type,
                got_ret: self.ret_type,
            })
    }
}

impl Clone for DynFn {
    fn clone(&self) -> Self {
        DynFn {
            boxed_fun: self.boxed_fun.clone_box(),
            ..*self
        }
    }
}

// the function with a statically known type
pub struct BoxedFn<Arg, Ret>(Box<dyn ClonableFn<Arg, Ret>>)
where
    Ret: EvalType;

impl<Arg, Ret> Clone for BoxedFn<Arg, Ret>
where
    Ret: EvalType,
{
    fn clone(&self) -> Self {
        self.clone_boxed()
    }
}

// implementing Deref allows to use the call syntax on the BoxedFn instances
impl<Arg, Ret> Deref for BoxedFn<Arg, Ret>
where
    Ret: EvalType,
{
    type Target = Box<dyn ClonableFn<Arg, Ret>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

trait ClonableAny: Any {
    fn clone_box(&self) -> Box<dyn ClonableAny>;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any + Clone> ClonableAny for T {
    fn clone_box(&self) -> Box<dyn ClonableAny> {
        Box::new(self.clone())
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub trait ClonableFn<Arg, Ret>:
    for<'a> Fn(&'a Arg) -> Ret::RefType<'a>
where
    Ret: EvalType,
{
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret>;
}

// implement ClonableFn for every matching function
impl<Arg, Ret, F> ClonableFn<Arg, Ret> for F
where
    Ret: EvalType,
    F: for<'a> Fn(&'a Arg) -> Ret::RefType<'a> + Clone + 'static,
{
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret> {
        BoxedFn(Box::new(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dyn_fn() {
        // here we construct a function that takes an (i64,i64) tuple and returns the first part
        // but the type of the variable just DynFn, no mention of tuples and i64
        let dyn_fn = DynFn::new::<_, i64>(|a: &(i64, i64)| a.0);

        // here we get back to the callable function with known types
        // but for that we need to know exact types at compile time
        let concrete_fn = dyn_fn.downcast::<(i64, i64), i64>().unwrap();

        // and here we call the downcasted function to test if it really works as intended
        assert_eq!((concrete_fn)(&(10, 20)), 10);
    }
}
