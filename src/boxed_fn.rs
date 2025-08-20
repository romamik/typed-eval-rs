use std::{any::Any, sync::Arc};

/// stores a function, allows to downcast back to the function knowing Arg and Ret types
/// can store functions of form fn(&Arg)->Ret and fn(&Arg)->&Ret
#[derive(Clone)]
pub struct DynBoxedFn(Arc<dyn Any>);

enum BoxedFn<Arg, Ret> {
    RetVal(BoxedFnRetVal<Arg, Ret>),
    RetRef(BoxedFnRetRef<Arg, Ret>),
}

pub type BoxedFnRetVal<Arg, Ret> = Arc<dyn Fn(&Arg) -> Ret>;
pub type BoxedFnRetRef<Arg, Ret> = Arc<dyn Fn(&Arg) -> &Ret>;

impl DynBoxedFn {
    pub fn make_ret_val<Arg, Ret>(f: impl Fn(&Arg) -> Ret + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self(Arc::new(BoxedFn::RetVal(Arc::new(f))))
    }

    pub fn make_ret_ref<Arg, Ret>(f: impl Fn(&Arg) -> &Ret + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self(Arc::new(BoxedFn::RetRef(Arc::new(f))))
    }

    pub fn downcast_ret_val<Arg, Ret>(&self) -> Option<BoxedFnRetVal<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.0
            .downcast_ref::<BoxedFn<Arg, Ret>>()
            .and_then(|boxed_fn| match boxed_fn {
                BoxedFn::RetVal(f) => Some(f.clone()),
                _ => None,
            })
    }

    pub fn downcast_ret_ref<Arg, Ret>(&self) -> Option<BoxedFnRetRef<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.0
            .downcast_ref::<BoxedFn<Arg, Ret>>()
            .and_then(|boxed_fn| match boxed_fn {
                BoxedFn::RetRef(f) => Some(f.clone()),
                _ => None,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let data = 100;

        let dyn_boxed_fn = DynBoxedFn::make_ret_val(|hello: &i32| *hello);
        let f = dyn_boxed_fn.downcast_ret_val::<i32, i32>().unwrap();
        assert_eq!(data, (f)(&data));

        let dyn_boxed_fn = DynBoxedFn::make_ret_ref(|hello: &i32| hello);
        let f = dyn_boxed_fn.downcast_ret_ref::<i32, i32>().unwrap();
        assert_eq!(data, *(f)(&data));
    }
}
