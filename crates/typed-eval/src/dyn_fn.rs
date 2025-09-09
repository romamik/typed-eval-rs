use std::{
    any::{Any, TypeId},
    ops::Deref,
};

use crate::SupportedType;

// the function with a dynamically known type
pub struct DynFn {
    pub arg_type: TypeId,
    pub ret_type: TypeId,
    boxed_fun: Box<dyn ClonableAny>,
}

impl DynFn {
    pub fn new<Arg, Ret>(
        f: impl for<'a> Fn(&'a Arg) -> Ret::RefType<'a> + Clone + 'static,
    ) -> Self
    where
        Arg: 'static,
        Ret: SupportedType,
    {
        Self {
            boxed_fun: Box::new(BoxedFn(Box::new(f))),
            arg_type: TypeId::of::<Arg>(),
            ret_type: TypeId::of::<Ret>(),
        }
    }

    pub fn downcast<Arg, Ret>(&self) -> Option<BoxedFn<Arg, Ret>>
    where
        Arg: 'static,
        Ret: SupportedType,
    {
        self.boxed_fun.as_any().downcast_ref().cloned()
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
    Ret: SupportedType;

impl<Arg, Ret> Clone for BoxedFn<Arg, Ret>
where
    Ret: SupportedType,
{
    fn clone(&self) -> Self {
        self.clone_boxed()
    }
}

// implementing Deref allows to use the call syntax on the BoxedFn instances
impl<Arg, Ret> Deref for BoxedFn<Arg, Ret>
where
    Ret: SupportedType,
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
    Ret: SupportedType,
{
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret>;
}

// implement ClonableFn for every matching function
impl<Arg, Ret, F> ClonableFn<Arg, Ret> for F
where
    Ret: SupportedType,
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
