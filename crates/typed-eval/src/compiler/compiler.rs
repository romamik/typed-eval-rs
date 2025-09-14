use crate::{
    BoxedFn, CompilerRegistry, DynFn, EvalType, Expr, MethodCallData,
    parse_expr,
};
use std::{any::TypeId, marker::PhantomData};

pub struct Compiler<Ctx> {
    registry: CompilerRegistry,
    ctx_ty: PhantomData<Ctx>,
}

impl<Ctx: EvalType> Compiler<Ctx> {
    pub fn new() -> Result<Self, String> {
        let mut registry = CompilerRegistry::default();

        // Register literal types
        registry.register_type::<Ctx, i64>()?;
        registry.register_type::<Ctx, f64>()?;

        // Register context type
        registry.register_type::<Ctx, Ctx>()?;

        Ok(Self {
            registry,
            ctx_ty: PhantomData,
        })
    }

    pub fn compile<Ret: EvalType>(
        &self,
        input: &str,
    ) -> Result<BoxedFn<Ctx, Ret>, String> {
        let dyn_fn = self.compile_dyn(input)?;
        let casted_fn = self.cast(dyn_fn, TypeId::of::<Ret>())?;
        casted_fn
            .downcast::<Ctx, Ret>()
            .ok_or_else(|| "Compiler error: type mismatch".to_string())
    }

    pub fn compile_dyn(&self, input: &str) -> Result<DynFn, String> {
        let expr = parse_expr(input);
        if expr.has_errors() || !expr.has_output() {
            let errors = expr
                .errors()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(",");
            Err(format!("Error parsing expression: {}", errors))?;
        }

        self.compile_expr(expr.output().unwrap())
    }

    fn compile_expr(&self, expr: &Expr) -> Result<DynFn, String> {
        match expr {
            &Expr::Int(val) => Ok(DynFn::new::<_, i64>(move |_ctx: &Ctx| val)),

            &Expr::Float(val) => {
                Ok(DynFn::new::<_, f64>(move |_ctx: &Ctx| val))
            }

            Expr::String(_) => Err("Strings literals are not supported".into()),

            Expr::Var(var_name) => self.compile_variable(var_name),

            Expr::UnOp(op, rhs) => self.compile_unary_op(op, rhs),

            Expr::BinOp(op, lhs, rhs) => self.compile_binary_op(op, lhs, rhs),

            Expr::FieldAccess(obj, field_name) => {
                let obj_fn = self.compile_expr(obj)?;
                self.compile_field_access(obj_fn, field_name)
            }

            Expr::FuncCall(func, args) => {
                self.compile_function_call(func, args)
            }
        }
    }

    /// try to cast expression to given type
    /// on success returned DynFn will have ret_type matching ty
    fn cast(&self, expr: DynFn, ty: TypeId) -> Result<DynFn, String> {
        if expr.ret_type == ty {
            return Ok(expr);
        }
        let key = (expr.ret_type, ty);
        let Some(compile_cast) = self.registry.casts.get(&key) else {
            Err(format!("Cannot cast from {:?} to {:?}", expr.ret_type, ty))?
        };
        compile_cast(expr)
    }

    /// try to cast two expressions so that they have the same ret_type
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
        Err("Cannot cast expressions to same type".into())
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
            Err(format!(
                "No such field '{field_name}' on type {:?}",
                object.ret_type
            ))?
        };
        compile_fn(object)
    }

    fn compile_method_call(
        &self,
        object: DynFn,
        method_name: &str,
        arguments: Vec<DynFn>,
    ) -> Result<DynFn, String> {
        let Some(MethodCallData {
            compile_fn,
            arg_types,
        }) = self
            .registry
            .method_calls
            .get(&(object.ret_type, method_name))
        else {
            Err(format!(
                "No such method '{}' on type {:?}",
                method_name, object.ret_type
            ))?
        };

        if arg_types.len() != arguments.len() {
            Err(format!(
                "Expected {} arguments, got {}",
                arg_types.len(),
                arguments.len()
            ))?;
        }

        // cast arguments to arg_types
        let arguments = arguments
            .into_iter()
            .zip(arg_types.iter().copied())
            .map(|(arg, ty)| self.cast(arg, ty))
            .collect::<Result<Vec<_>, String>>()?;

        compile_fn(object, arguments)
    }

    fn compile_unary_op(
        &self,
        op: &crate::UnOp,
        rhs: &Expr,
    ) -> Result<DynFn, String> {
        let rhs_fn = self.compile_expr(rhs)?;
        let Some(compile_fn) =
            self.registry.unary_operations.get(&(*op, rhs_fn.ret_type))
        else {
            Err(format!("Unsupported unary operation {:?}", op))?
        };
        compile_fn(rhs_fn)
    }

    fn compile_binary_op(
        &self,
        op: &crate::BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<DynFn, String> {
        let lhs_fn = self.compile_expr(lhs)?;
        let rhs_fn = self.compile_expr(rhs)?;
        let (lhs_fn, rhs_fn) = self.cast_same_type(lhs_fn, rhs_fn)?;

        let Some(compile_fn) =
            self.registry.binary_operations.get(&(*op, lhs_fn.ret_type))
        else {
            Err(format!("Unsupported binary operation {:?}", op))?
        };
        compile_fn(lhs_fn, rhs_fn)
    }

    fn compile_variable(&self, var_name: &str) -> Result<DynFn, String> {
        let ctx_fn = Ctx::make_dyn_fn(|ctx: &Ctx| Ctx::to_ref_type(ctx));
        self.compile_field_access(ctx_fn, var_name)
    }

    fn compile_function_call(
        &self,
        function: &Expr,
        arguments: &[Expr],
    ) -> Result<DynFn, String> {
        let args_fns = arguments
            .iter()
            .map(|arg| self.compile_expr(arg))
            .collect::<Result<Vec<_>, String>>()?;

        let ctx_fn = Ctx::make_dyn_fn(|ctx: &Ctx| Ctx::to_ref_type(ctx));

        match function {
            Expr::Var(var_name) => {
                self.compile_method_call(ctx_fn, var_name, args_fns)
            }
            Expr::FieldAccess(obj, field_name) => {
                let obj_fn = self.compile_expr(obj)?;
                self.compile_method_call(obj_fn, field_name, args_fns)
            }
            _ => Err("Unsupported function call".into()),
        }
    }
}
