use crate::{BinOp, DynFn, RegistryAccess, Result, TypeInfo, UnOp};
use std::borrow::Cow;

pub trait EvalType: EvalTypeMethods + 'static {
    type RefType<'a>;

    fn type_info() -> TypeInfo;

    fn to_ref_type<'a>(&'a self) -> Self::RefType<'a>;

    fn register<Ctx: EvalType>(
        registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()>;

    fn make_dyn_fn<Ctx: EvalType>(
        f: impl for<'a> Fn(&'a Ctx) -> Self::RefType<'a> + Clone + 'static,
    ) -> DynFn {
        DynFn::new::<Ctx, Self>(f)
    }
}

// separate trait for methods
// because Derive macro do not have access to impl blocks
// there is a separate procedural macro to implement this trait
pub trait EvalTypeMethods: Sized {
    fn register_methods<Ctx: EvalType>(
        registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        _ = registry;
        Ok(())
    }
}

#[cfg(feature = "nightly")]
// Blanket implementation with no methods
impl<T: EvalType> EvalTypeMethods for T {
    default fn register_methods<Ctx: EvalType>(
        _registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        Ok(())
    }
}

#[cfg(not(feature = "nightly"))]
impl EvalTypeMethods for () {}
impl EvalType for () {
    type RefType<'a> = ();

    fn type_info() -> TypeInfo {
        TypeInfo::new::<Self>("void")
    }

    fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
        *self
    }

    fn register<Ctx: EvalType>(
        _registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        Ok(())
    }
}

#[cfg(not(feature = "nightly"))]
impl EvalTypeMethods for i64 {}
impl EvalType for i64 {
    type RefType<'a> = i64;

    fn type_info() -> TypeInfo {
        TypeInfo::new::<Self>("i64")
    }

    fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
        *self
    }

    fn register<Ctx: EvalType>(
        mut registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        registry.register_cast::<f64>(|_: &Ctx, value: i64| value as f64)?;
        registry.register_cast::<String>(|_: &Ctx, value: i64| {
            format!("{value}").into()
        })?;

        registry
            .register_bin_op(BinOp::Add, |_: &Ctx, lhs: i64, rhs: i64| {
                lhs + rhs
            })?;
        registry
            .register_bin_op(BinOp::Sub, |_: &Ctx, lhs: i64, rhs: i64| {
                lhs - rhs
            })?;
        registry
            .register_bin_op(BinOp::Mul, |_: &Ctx, lhs: i64, rhs: i64| {
                lhs * rhs
            })?;
        registry
            .register_bin_op(BinOp::Div, |_: &Ctx, lhs: i64, rhs: i64| {
                lhs / rhs
            })?;
        registry.register_un_op(UnOp::Neg, |_: &Ctx, rhs: i64| -rhs)?;
        registry.register_un_op(UnOp::Plus, |_: &Ctx, rhs: i64| rhs)?;
        Ok(())
    }
}

#[cfg(not(feature = "nightly"))]
impl EvalTypeMethods for f64 {}
impl EvalType for f64 {
    type RefType<'a> = f64;

    fn type_info() -> TypeInfo {
        TypeInfo::new::<Self>("f64")
    }

    fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
        *self
    }

    fn register<Ctx: EvalType>(
        mut registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        registry.register_cast::<String>(|_: &Ctx, value: f64| {
            format!("{value}").into()
        })?;

        registry
            .register_bin_op(BinOp::Add, |_: &Ctx, lhs: f64, rhs: f64| {
                lhs + rhs
            })?;
        registry
            .register_bin_op(BinOp::Sub, |_: &Ctx, lhs: f64, rhs: f64| {
                lhs - rhs
            })?;
        registry
            .register_bin_op(BinOp::Mul, |_: &Ctx, lhs: f64, rhs: f64| {
                lhs * rhs
            })?;
        registry
            .register_bin_op(BinOp::Div, |_: &Ctx, lhs: f64, rhs: f64| {
                lhs / rhs
            })?;
        registry.register_un_op(UnOp::Neg, |_: &Ctx, rhs: f64| -rhs)?;
        registry.register_un_op(UnOp::Plus, |_: &Ctx, rhs: f64| rhs)?;
        Ok(())
    }
}

#[cfg(not(feature = "nightly"))]
impl EvalTypeMethods for String {}
impl EvalType for String {
    type RefType<'a> = Cow<'a, str>;

    fn type_info() -> TypeInfo {
        TypeInfo::new::<Self>("String")
    }

    fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
        self.into()
    }

    fn register<Ctx: EvalType>(
        mut registry: RegistryAccess<Ctx, Self>,
    ) -> Result<()> {
        registry.register_bin_op(
            BinOp::Add,
            |_: &Ctx, lhs: Self::RefType<'_>, rhs: Self::RefType<'_>| lhs + rhs,
        )?;
        Ok(())
    }
}

macro_rules! impl_eval_type_for_tuples {
    // Match one or more types in a tuple
    ( $( $name:ident ),+ ) => {
        #[cfg(not(feature = "nightly"))]
        impl< $( $name ),+ > EvalTypeMethods for ( $( $name ),+ ) {}

        impl< $( $name ),+ > EvalType for ( $( $name ),+ )
        where
            $( $name: EvalType ),+
        {
            type RefType<'a> = &'a Self;

            fn type_info() -> TypeInfo {
                TypeInfo::new::<Self>(concat!("Tuple(", $(stringify!($name), ","),*, ")"))
            }

            fn to_ref_type<'a>(&'a self) -> Self::RefType<'a> {
                self
            }

            fn register<Ctx: EvalType>(
                _registry: RegistryAccess<Ctx, Self>,
            ) -> Result<()> {
                Ok(())
            }
        }
    };
}

impl_eval_type_for_tuples!(T1, T2);
impl_eval_type_for_tuples!(T1, T2, T3);
impl_eval_type_for_tuples!(T1, T2, T3, T4);
impl_eval_type_for_tuples!(T1, T2, T3, T4, T5);
