use std::{
    any::{Any, TypeId},
    ops::Deref,
};

// the function with a dynamically known type
pub struct DynFn {
    pub arg_type: TypeId,
    pub ret_type: TypeId,
    boxed_fun: Box<dyn ClonableAny>,
}

impl DynFn {
    pub fn new<Arg, Ret>(f: impl Fn(&Arg) -> Ret + Clone + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
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
        Ret: 'static,
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
pub struct BoxedFn<Arg, Ret>(Box<dyn ClonableFn<Arg, Ret>>);

impl<Arg, Ret> Clone for BoxedFn<Arg, Ret> {
    fn clone(&self) -> Self {
        self.clone_boxed()
    }
}

// implementing Deref allows to use the call syntax on the BoxedFn instances
impl<Arg, Ret> Deref for BoxedFn<Arg, Ret> {
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

pub trait ClonableFn<Arg, Ret>: Fn(&Arg) -> Ret {
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret>;
}

// implement ClonableFn for every matching function
impl<Arg, Ret, F> ClonableFn<Arg, Ret> for F
where
    F: Fn(&Arg) -> Ret + Clone + 'static,
{
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret> {
        BoxedFn(Box::new(self.clone()))
    }
}
