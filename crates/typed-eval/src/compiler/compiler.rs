use crate::{
    BoxedFn, CompilerRegistry, DynFn, Error, EvalType, Expr, MethodCallData,
    Result, TypeInfo, parse_expr,
};
use std::marker::PhantomData;

pub struct Compiler<Ctx> {
    registry: CompilerRegistry,
    ctx_ty: PhantomData<Ctx>,
}

impl<Ctx: EvalType> Compiler<Ctx> {
    pub fn new() -> Result<Self> {
        let mut registry = CompilerRegistry::default();

        // Register literal types
        registry.register_type::<Ctx, i64>()?;
        registry.register_type::<Ctx, f64>()?;
        registry.register_type::<Ctx, String>()?;

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
    ) -> Result<BoxedFn<Ctx, Ret>> {
        let dyn_fn = self.compile_dyn(input)?;
        let casted_fn = self.cast(dyn_fn, Ret::type_info())?;
        casted_fn.downcast::<Ctx, Ret>()
    }

    pub fn compile_dyn(&self, input: &str) -> Result<DynFn> {
        let expr = parse_expr(input);
        if expr.has_errors() || !expr.has_output() {
            let errors = expr
                .errors()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(",");
            Err(Error::ParseError(errors))?;
        }

        self.compile_expr(expr.output().unwrap())
    }

    fn compile_expr(&self, expr: &Expr) -> Result<DynFn> {
        match expr {
            &Expr::Int(val) => Ok(DynFn::new::<_, i64>(move |_ctx: &Ctx| val)),

            &Expr::Float(val) => {
                Ok(DynFn::new::<_, f64>(move |_ctx: &Ctx| val))
            }

            Expr::String(s) => {
                let s = s.clone();
                Ok(DynFn::new::<_, String>(move |_ctx: &Ctx| s.clone().into()))
            }

            Expr::Var(var_name) => self.compile_variable(var_name),

            Expr::UnOp(op, rhs) => self.compile_unary_op(*op, rhs),

            Expr::BinOp(op, lhs, rhs) => self.compile_binary_op(*op, lhs, rhs),

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
    fn cast(&self, expr: DynFn, ty: TypeInfo) -> Result<DynFn> {
        if expr.ret_type == ty {
            return Ok(expr);
        }
        let key = (expr.ret_type, ty);
        let Some(compile_cast) = self.registry.casts.get(&key) else {
            Err(Error::CantCast {
                from: expr.ret_type,
                to: ty,
            })?
        };
        compile_cast(expr)
    }

    /// try to cast two expressions so that they have the same ret_type
    fn cast_same_type(&self, a: DynFn, b: DynFn) -> Result<(DynFn, DynFn)> {
        if a.ret_type == b.ret_type {
            return Ok((a, b));
        }

        if let Ok(b_casted) = self.cast(b.clone(), a.ret_type) {
            return Ok((a, b_casted));
        }

        if let Ok(a_casted) = self.cast(a.clone(), b.ret_type) {
            return Ok((a_casted, b));
        }

        Err(Error::CantCastSameType(a.ret_type, b.ret_type))?
    }

    fn compile_field_access(
        &self,
        object: DynFn,
        field: &str,
    ) -> Result<DynFn> {
        let Some(compile_fn) =
            self.registry.field_access.get(&(object.ret_type, field))
        else {
            Err(Error::FieldNotFound {
                ty: object.ret_type,
                field: field.into(),
            })?
        };
        compile_fn(object)
    }

    fn compile_method_call(
        &self,
        object: DynFn,
        method: &str,
        arguments: Vec<DynFn>,
    ) -> Result<DynFn> {
        let Some(MethodCallData {
            compile_fn,
            arg_types,
        }) = self.registry.method_calls.get(&(object.ret_type, method))
        else {
            Err(Error::MethodNotFound {
                ty: object.ret_type,
                method: method.into(),
            })?
        };

        if arg_types.len() != arguments.len() {
            Err(Error::ArgCountMismatch {
                expected: arg_types.len(),
                got: arguments.len(),
            })?;
        }

        // cast arguments to arg_types
        let arguments = arguments
            .into_iter()
            .zip(arg_types.iter().copied())
            .map(|(arg, ty)| self.cast(arg, ty))
            .collect::<Result<Vec<_>>>()?;

        compile_fn(object, arguments)
    }

    fn compile_unary_op(&self, op: crate::UnOp, rhs: &Expr) -> Result<DynFn> {
        let rhs_fn = self.compile_expr(rhs)?;

        let ty = rhs_fn.ret_type;

        let Some(compile_fn) = self.registry.unary_operations.get(&(op, ty))
        else {
            Err(Error::UnknownUnaryOp { op, ty })?
        };

        compile_fn(rhs_fn)
    }

    fn compile_binary_op(
        &self,
        op: crate::BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<DynFn> {
        let lhs_fn = self.compile_expr(lhs)?;
        let rhs_fn = self.compile_expr(rhs)?;

        let (lhs_fn, rhs_fn) = self.cast_same_type(lhs_fn, rhs_fn)?;
        let ty = lhs_fn.ret_type;

        let Some(compile_fn) = self.registry.binary_operations.get(&(op, ty))
        else {
            Err(Error::UnknownBinaryOp { op, ty })?
        };

        compile_fn(lhs_fn, rhs_fn)
    }

    fn compile_variable(&self, var_name: &str) -> Result<DynFn> {
        let ctx_fn = Ctx::make_dyn_fn(|ctx: &Ctx| Ctx::to_ref_type(ctx));
        self.compile_field_access(ctx_fn, var_name)
    }

    fn compile_function_call(
        &self,
        function: &Expr,
        arguments: &[Expr],
    ) -> Result<DynFn> {
        let args_fns = arguments
            .iter()
            .map(|arg| self.compile_expr(arg))
            .collect::<Result<Vec<_>>>()?;

        let ctx_fn = Ctx::make_dyn_fn(|ctx: &Ctx| Ctx::to_ref_type(ctx));

        match function {
            Expr::Var(var_name) => {
                self.compile_method_call(ctx_fn, var_name, args_fns)
            }
            Expr::FieldAccess(obj, field_name) => {
                let obj_fn = self.compile_expr(obj)?;
                self.compile_method_call(obj_fn, field_name, args_fns)
            }
            _ => Err(Error::UnsupportedFunctionCall)?,
        }
    }
}
