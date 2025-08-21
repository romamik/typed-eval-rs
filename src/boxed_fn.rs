use crate::tdesc::TDesc;
use std::any::Any;

/// stores a function, allows to downcast back to the function knowing Arg and Ret types
/// can store functions of form fn(&Arg)->Ret and fn(&Arg)->&Ret
pub struct DynBoxedFn {
    boxed_fn: Box<dyn Any>,
    pub arg_type: TDesc,
    pub ret_type: RetType,
}

impl DynBoxedFn {
    pub fn make_ret_val<Arg, Ret>(f: impl Fn(&Arg) -> Ret + Clone + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            boxed_fn: Box::new(BoxedFn::RetVal(BoxedFnRetVal::new(f))),
            arg_type: TDesc::of::<Arg>(),
            ret_type: RetType::of_val::<Ret>(),
        }
    }

    pub fn make_ret_ref<Arg, Ret>(f: impl Fn(&Arg) -> &Ret + Clone + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            boxed_fn: Box::new(BoxedFn::RetRef(BoxedFnRetRef::new(f))),
            arg_type: TDesc::of::<Arg>(),
            ret_type: RetType::of_ref::<Ret>(),
        }
    }

    pub fn get_fn_ret_val<Arg, Ret>(&self) -> DowncastResult<BoxedFnRetVal<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.boxed_fn
            .downcast_ref::<BoxedFn<Arg, Ret>>()
            .and_then(|boxed_fn| boxed_fn.get_fn_ret_val())
            .ok_or_else(|| DowncastError::new::<Arg, Ret>(self, false))
    }

    pub fn get_fn_ret_ref<Arg, Ret>(&self) -> DowncastResult<BoxedFnRetRef<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.boxed_fn
            .downcast_ref::<BoxedFn<Arg, Ret>>()
            .and_then(|boxed_fn| boxed_fn.get_fn_ret_ref())
            .ok_or_else(|| DowncastError::new::<Arg, Ret>(self, true))
    }
}

enum BoxedFn<Arg, Ret> {
    RetVal(BoxedFnRetVal<Arg, Ret>),
    RetRef(BoxedFnRetRef<Arg, Ret>),
}

impl<Arg, Ret> BoxedFn<Arg, Ret> {
    fn get_fn_ret_val(&self) -> Option<BoxedFnRetVal<Arg, Ret>> {
        match self {
            Self::RetVal(boxed_fn) => Some(boxed_fn.clone()),
            _ => None,
        }
    }

    fn get_fn_ret_ref(&self) -> Option<BoxedFnRetRef<Arg, Ret>> {
        match self {
            Self::RetRef(boxed_fn) => Some(boxed_fn.clone()),
            _ => None,
        }
    }
}

pub struct BoxedFnRetVal<Arg, Ret>(Box<dyn BoxedFnRetValTrait<Arg, Ret>>);

impl<Arg, Ret> BoxedFnRetVal<Arg, Ret> {
    fn new<F>(f: F) -> Self
    where
        F: Fn(&Arg) -> Ret + Clone + 'static,
    {
        Self(Box::new(f))
    }

    pub fn call(&self, arg: &Arg) -> Ret {
        self.0.call(arg)
    }
}

impl<Arg, Ret> Clone for BoxedFnRetVal<Arg, Ret> {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}

trait BoxedFnRetValTrait<Arg, Ret> {
    fn clone_box(&self) -> Box<dyn BoxedFnRetValTrait<Arg, Ret>>;
    fn call(&self, arg: &Arg) -> Ret;
}

impl<Arg, Ret, F> BoxedFnRetValTrait<Arg, Ret> for F
where
    F: Fn(&Arg) -> Ret + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn BoxedFnRetValTrait<Arg, Ret>> {
        Box::new(self.clone())
    }

    fn call(&self, arg: &Arg) -> Ret {
        (self)(arg)
    }
}

pub struct BoxedFnRetRef<Arg, Ret>(Box<dyn BoxedFnRetRefTrait<Arg, Ret>>);

impl<Arg, Ret> Clone for BoxedFnRetRef<Arg, Ret> {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}

impl<Arg, Ret> BoxedFnRetRef<Arg, Ret> {
    fn new<F>(f: F) -> Self
    where
        F: Fn(&Arg) -> &Ret + Clone + 'static,
    {
        Self(Box::new(f))
    }

    pub fn call<'a>(&self, arg: &'a Arg) -> &'a Ret {
        self.0.call(arg)
    }
}

trait BoxedFnRetRefTrait<Arg, Ret> {
    fn clone_box(&self) -> Box<dyn BoxedFnRetRefTrait<Arg, Ret>>;
    fn call<'a>(&self, arg: &'a Arg) -> &'a Ret;
}

impl<Arg, Ret, F> BoxedFnRetRefTrait<Arg, Ret> for F
where
    F: Fn(&Arg) -> &Ret + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn BoxedFnRetRefTrait<Arg, Ret>> {
        Box::new(self.clone())
    }

    fn call<'a>(&self, arg: &'a Arg) -> &'a Ret {
        (self)(arg)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RetType {
    pub ty: TDesc,
    pub is_ref: bool,
}

impl RetType {
    pub fn of_val<T>() -> Self
    where
        T: 'static,
    {
        Self {
            ty: TDesc::of::<T>(),
            is_ref: false,
        }
    }

    pub fn of_ref<T>() -> Self
    where
        T: 'static,
    {
        Self {
            ty: TDesc::of::<T>().to_owned(),
            is_ref: true,
        }
    }
}

impl std::fmt::Display for RetType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_ref {
            write!(f, "&")?
        }
        write!(f, "{}", self.ty)
    }
}

impl std::fmt::Debug for RetType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

pub struct DowncastError {
    pub data: Box<DowncastErrorData>,
}

pub struct DowncastErrorData {
    pub want_arg: TDesc,
    pub want_ret: RetType,
    pub have_arg: TDesc,
    pub have_ret: RetType,
}

pub type DowncastResult<T> = Result<T, DowncastError>;

impl DowncastError {
    fn new<Arg, Ret>(dyn_boxed_fn: &DynBoxedFn, want_ret_ref: bool) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            data: Box::new(DowncastErrorData {
                have_arg: dyn_boxed_fn.arg_type,
                have_ret: dyn_boxed_fn.ret_type,
                want_arg: TDesc::of::<Arg>(),
                want_ret: if want_ret_ref {
                    RetType::of_ref::<Ret>()
                } else {
                    RetType::of_val::<Ret>()
                },
            }),
        }
    }
}

impl std::fmt::Display for DowncastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DynBoxFn downcast failed. ")?;
        writeln!(
            f,
            "Want Arg {}, Ret {}. ",
            self.data.want_arg, self.data.want_ret
        )?;
        writeln!(
            f,
            "Have Arg {}, Ret {}. ",
            self.data.have_arg, self.data.have_ret
        )?;

        Ok(())
    }
}

impl std::fmt::Debug for DowncastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl From<DowncastError> for String {
    fn from(value: DowncastError) -> Self {
        value.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_test() {
        let data = 100;

        let dyn_boxed_fn = DynBoxedFn::make_ret_val(|hello: &i32| *hello);
        let f = dyn_boxed_fn.get_fn_ret_val::<i32, i32>().unwrap();
        assert_eq!(data, f.call(&data));
        assert!(dyn_boxed_fn.get_fn_ret_ref::<i32, i32>().is_err());
        assert!(dyn_boxed_fn.get_fn_ret_ref::<i32, f32>().is_err());

        let dyn_boxed_fn = DynBoxedFn::make_ret_ref(|hello: &i32| hello);
        let f = dyn_boxed_fn.get_fn_ret_ref::<i32, i32>().unwrap();
        assert_eq!(data, *f.call(&data));
        assert!(dyn_boxed_fn.get_fn_ret_val::<i32, i32>().is_err());
        assert!(dyn_boxed_fn.get_fn_ret_val::<i32, f32>().is_err());
    }
}
