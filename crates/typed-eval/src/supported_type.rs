use crate::{BinOp, DynFn, RegistryAccess, UnOp};

pub trait SupportedType: Sized + 'static {
    type RefType<'a>;

    fn register<Ctx: SupportedType>(
        registry: RegistryAccess<Ctx, Self>,
    ) -> Result<(), String>;

    fn make_dyn_fn<Ctx>(
        f: impl for<'a> Fn(&'a Ctx) -> Self::RefType<'a> + Clone + 'static,
    ) -> DynFn
    where
        Ctx: 'static,
    {
        DynFn::new::<Ctx, Self>(f)
    }
}

impl SupportedType for i64 {
    type RefType<'a> = i64;

    fn register<Ctx: SupportedType>(
        mut registry: RegistryAccess<Ctx, Self>,
    ) -> Result<(), String> {
        registry
            .register_cast::<i64, f64>(|_: &Ctx, value: i64| value as f64)?;

        registry.register_bin_op::<i64>(
            BinOp::Add,
            |_: &Ctx, lhs: i64, rhs: i64| lhs + rhs,
        )?;
        registry.register_bin_op::<i64>(
            BinOp::Sub,
            |_: &Ctx, lhs: i64, rhs: i64| lhs - rhs,
        )?;
        registry.register_bin_op::<i64>(
            BinOp::Mul,
            |_: &Ctx, lhs: i64, rhs: i64| lhs * rhs,
        )?;
        registry.register_bin_op::<i64>(
            BinOp::Div,
            |_: &Ctx, lhs: i64, rhs: i64| lhs / rhs,
        )?;
        registry.register_un_op::<i64>(UnOp::Neg, |_: &Ctx, rhs: i64| -rhs)?;
        registry.register_un_op::<i64>(UnOp::Plus, |_: &Ctx, rhs: i64| rhs)?;
        Ok(())
    }
}

impl SupportedType for f64 {
    type RefType<'a> = f64;

    fn register<Ctx: SupportedType>(
        mut registry: RegistryAccess<Ctx, Self>,
    ) -> Result<(), String> {
        registry.register_bin_op::<f64>(
            BinOp::Add,
            |_: &Ctx, lhs: f64, rhs: f64| lhs + rhs,
        )?;
        registry.register_bin_op::<f64>(
            BinOp::Sub,
            |_: &Ctx, lhs: f64, rhs: f64| lhs - rhs,
        )?;
        registry.register_bin_op::<f64>(
            BinOp::Mul,
            |_: &Ctx, lhs: f64, rhs: f64| lhs * rhs,
        )?;
        registry.register_bin_op::<f64>(
            BinOp::Div,
            |_: &Ctx, lhs: f64, rhs: f64| lhs / rhs,
        )?;
        registry.register_un_op::<f64>(UnOp::Neg, |_: &Ctx, rhs: f64| -rhs)?;
        registry.register_un_op::<f64>(UnOp::Plus, |_: &Ctx, rhs: f64| rhs)?;
        Ok(())
    }
}
