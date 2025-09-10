use crate::{BoxedFn, CompilerRegistry, DynFn, Expr, SupportedType};
use std::{any::TypeId, marker::PhantomData};

pub trait ExprContext: SupportedType {
    fn get_self_dyn_fn() -> DynFn;
}

impl<T> ExprContext for T
where
    T: for<'a> SupportedType<RefType<'a> = &'a T>,
{
    fn get_self_dyn_fn() -> DynFn {
        T::make_dyn_fn(move |ctx| ctx)
    }
}

pub struct Compiler<Ctx> {
    registry: CompilerRegistry,
    ctx_ty: PhantomData<Ctx>,
}

impl<Ctx: ExprContext> Compiler<Ctx> {
    pub fn new() -> Result<Self, String> {
        let mut registry = CompilerRegistry::default();

        // register literal types
        registry.register_type::<Ctx, i64>()?;
        registry.register_type::<Ctx, f64>()?;

        // register context type and all types referenced by it
        registry.register_type::<Ctx, Ctx>()?;

        Ok(Self {
            registry,
            ctx_ty: PhantomData,
        })
    }

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

    fn compile_field_access(
        &self,
        object: DynFn,
        field_name: &str,
    ) -> Result<DynFn, String> {
        let Some(compile_fn) = self
            .registry
            .field_access
            .get(&(object.ret_type, field_name))
        else {
            Err(format!("No such field {field_name}"))?
        };
        compile_fn(object)
    }

    fn compile_method_call(
        &self,
        object: DynFn,
        method_name: &str,
        arguments: Vec<DynFn>,
    ) -> Result<DynFn, String> {
        let Some(compile_fn) = self.registry.method_calls.get(&(
            object.ret_type,
            method_name,
            arguments.iter().map(|arg| arg.ret_type).collect::<Vec<_>>(),
        )) else {
            Err("No such method".to_string())?
        };
        compile_fn(object, arguments)
    }

    pub fn compile_expr(&self, expr: &Expr) -> Result<DynFn, String> {
        Ok(match expr {
            &Expr::Int(val) => DynFn::new::<_, i64>(move |_ctx: &Ctx| val),
            &Expr::Float(val) => DynFn::new::<_, f64>(move |_ctx: &Ctx| val),
            Expr::String(_string) => Err("Strings not supported")?,
            Expr::Var(var_name) => {
                self.compile_field_access(Ctx::get_self_dyn_fn(), var_name)?
            }
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
            Expr::FieldAccess(object, field_name) => {
                let object = self.compile_expr(object)?;
                self.compile_field_access(object, field_name)?
            }
            Expr::FuncCall(function, arguments) => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, String>>()?;
                match function.as_ref() {
                    Expr::Var(var_name) => self.compile_method_call(
                        Ctx::get_self_dyn_fn(),
                        var_name,
                        arguments,
                    )?,
                    Expr::FieldAccess(object, field_name) => {
                        let object = self.compile_expr(object)?;
                        self.compile_method_call(object, field_name, arguments)?
                    }
                    _ => return Err("Unsupported function call".into()),
                }
            }
        })
    }

    pub fn compile<Ret: SupportedType>(
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
