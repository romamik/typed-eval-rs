use crate::{BinOp, CompilerRegistry, UnOp};

pub trait SupportedType: Clone + 'static {
    fn register<Ctx: SupportedType>(registry: &mut CompilerRegistry<Ctx>);
}

impl SupportedType for i64 {
    fn register<Ctx: SupportedType>(registry: &mut CompilerRegistry<Ctx>) {
        registry.register_cast(|value: i64| value as f64);

        registry.register_bin_op(BinOp::Add, |lhs: i64, rhs: i64| lhs + rhs);
        registry.register_bin_op(BinOp::Sub, |lhs: i64, rhs: i64| lhs - rhs);
        registry.register_bin_op(BinOp::Mul, |lhs: i64, rhs: i64| lhs * rhs);
        registry.register_bin_op(BinOp::Div, |lhs: i64, rhs: i64| lhs / rhs);
        registry.register_un_op(UnOp::Neg, |rhs: i64| -rhs);
        registry.register_un_op(UnOp::Plus, |rhs: i64| rhs);
    }
}

impl SupportedType for f64 {
    fn register<Ctx: SupportedType>(registry: &mut CompilerRegistry<Ctx>) {
        registry.register_bin_op(BinOp::Add, |lhs: f64, rhs: f64| lhs + rhs);
        registry.register_bin_op(BinOp::Sub, |lhs: f64, rhs: f64| lhs - rhs);
        registry.register_bin_op(BinOp::Mul, |lhs: f64, rhs: f64| lhs * rhs);
        registry.register_bin_op(BinOp::Div, |lhs: f64, rhs: f64| lhs / rhs);
        registry.register_un_op(UnOp::Neg, |rhs: f64| -rhs);
        registry.register_un_op(UnOp::Plus, |rhs: f64| rhs);
    }
}
