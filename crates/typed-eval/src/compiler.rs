use crate::{BinOp, BoxedFn, CompilerRegistry, DynFn, Expr, UnOp};
use std::any::TypeId;

pub trait ExprContext: 'static {
    // returns a function that takes Self as argument
    fn field_getter(field_name: &str) -> Option<DynFn>;
}

pub struct Compiler<Ctx> {
    registry: CompilerRegistry<Ctx>,
}

impl<Ctx: ExprContext> Default for Compiler<Ctx> {
    fn default() -> Self {
        let mut registry = CompilerRegistry::default();

        registry.register_cast(|value: i64| value as f64);

        registry.register_bin_op(BinOp::Add, |lhs: i64, rhs: i64| lhs + rhs);
        registry.register_bin_op(BinOp::Sub, |lhs: i64, rhs: i64| lhs - rhs);
        registry.register_bin_op(BinOp::Mul, |lhs: i64, rhs: i64| lhs * rhs);
        registry.register_bin_op(BinOp::Div, |lhs: i64, rhs: i64| lhs / rhs);
        registry.register_un_op(UnOp::Neg, |rhs: i64| -rhs);
        registry.register_un_op(UnOp::Plus, |rhs: i64| rhs);

        registry.register_bin_op(BinOp::Add, |lhs: f64, rhs: f64| lhs + rhs);
        registry.register_bin_op(BinOp::Sub, |lhs: f64, rhs: f64| lhs - rhs);
        registry.register_bin_op(BinOp::Mul, |lhs: f64, rhs: f64| lhs * rhs);
        registry.register_bin_op(BinOp::Div, |lhs: f64, rhs: f64| lhs / rhs);
        registry.register_un_op(UnOp::Neg, |rhs: f64| -rhs);
        registry.register_un_op(UnOp::Plus, |rhs: f64| rhs);

        Self { registry }
    }
}

impl<Ctx: ExprContext> Compiler<Ctx> {
    // helper function that tries to cast expression to given type
    fn cast(&self, expr: DynFn, ty: TypeId) -> Result<DynFn, String> {
        if expr.ret_type == ty {
            return Ok(expr);
        }
        let key = (expr.ret_type, ty);
        let Some(compile_cast_func) = self.registry.casts.get(&key) else {
            Err("Cannot cast")?
        };
        compile_cast_func(expr)
    }

    // helper functions that tries to make two expressions the same type
    fn cast_same_type(
        &self,
        a: DynFn,
        b: DynFn,
    ) -> Result<(DynFn, DynFn), String> {
        if a.ret_type == b.ret_type {
            return Ok((a, b));
        }
        if let Ok(b_casted) = self.cast(b.clone(), a.ret_type) {
            return Ok((a, b_casted));
        }
        if let Ok(a_casted) = self.cast(a, b.ret_type) {
            return Ok((a_casted, b));
        }
        Err("Cannot cast to same type".to_string())
    }

    pub fn compile_expr(&self, expr: &Expr) -> Result<DynFn, String> {
        Ok(match expr {
            &Expr::Int(val) => DynFn::new(move |_ctx: &Ctx| val),
            &Expr::Float(val) => DynFn::new(move |_ctx: &Ctx| val),
            Expr::String(_string) => Err("Strings not supported")?,
            Expr::Var(var_name) => Ctx::field_getter(var_name)
                .ok_or(format!("Unknown variable ${var_name}"))?,
            Expr::UnOp(op, rhs) => {
                let rhs = self.compile_expr(rhs)?;

                let Some(compile_un_op) =
                    self.registry.unary_operations.get(&(*op, rhs.ret_type))
                else {
                    Err("Unsupported unary operation")?
                };

                compile_un_op(rhs)?
            }
            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                let (lhs, rhs) = self.cast_same_type(lhs, rhs)?;

                let Some(compile_bin_op) =
                    self.registry.binary_operations.get(&(*op, lhs.ret_type))
                else {
                    Err("Unsupported binary operation")?
                };

                compile_bin_op(lhs, rhs)?
            }
            Expr::FieldAccess(_object, _field_name) => {
                Err("Field access not supported")?
            }
            Expr::FuncCall(_function, _arguments) => {
                Err("Function calls not supported")?
            }
        })
    }

    pub fn compile<Ret: 'static>(
        &self,
        expr: &Expr,
    ) -> Result<BoxedFn<Ctx, Ret>, String> {
        let dyn_fn = self.compile_expr(expr)?;
        let casted_dyn_fn = self.cast(dyn_fn, TypeId::of::<Ret>())?;
        casted_dyn_fn
            .downcast::<Ctx, Ret>()
            .ok_or("Compiler error: type mismatch".to_string())
    }
}
