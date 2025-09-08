use crate::{BinOp, BoxedFn, DynFn, Expr, UnOp};
use std::{any::TypeId, collections::HashMap, marker::PhantomData};

pub trait ExprContext: 'static {
    // returns a function that takes Self as argument
    fn field_getter(field_name: &str) -> Option<DynFn>;
}

type UnOpKey = (UnOp, TypeId);
type CompileUnOpFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type BinOpKey = (BinOp, TypeId);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn, String>>;

type CastKey = (TypeId, TypeId);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type FieldAccessKey = (TypeId, &'static str);
type FieldAccessFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

pub struct Compiler<Ctx> {
    casts: HashMap<CastKey, CompileCastFunc>,
    unary_operations: HashMap<UnOpKey, CompileUnOpFunc>,
    binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    field_access: HashMap<FieldAccessKey, FieldAccessFunc>,
    ctx_type: PhantomData<Ctx>,
}

impl<Ctx: ExprContext> Default for Compiler<Ctx> {
    fn default() -> Self {
        let mut compiler = Self {
            casts: HashMap::new(),
            unary_operations: HashMap::new(),
            binary_operations: HashMap::new(),
            field_access: HashMap::new(),
            ctx_type: PhantomData,
        };

        compiler.register_cast(|value: i64| value as f64);

        compiler.register_bin_op(BinOp::Add, |lhs: i64, rhs: i64| lhs + rhs);
        compiler.register_bin_op(BinOp::Sub, |lhs: i64, rhs: i64| lhs - rhs);
        compiler.register_bin_op(BinOp::Mul, |lhs: i64, rhs: i64| lhs * rhs);
        compiler.register_bin_op(BinOp::Div, |lhs: i64, rhs: i64| lhs / rhs);
        compiler.register_un_op(UnOp::Neg, |rhs: i64| -rhs);
        compiler.register_un_op(UnOp::Plus, |rhs: i64| rhs);

        compiler.register_bin_op(BinOp::Add, |lhs: f64, rhs: f64| lhs + rhs);
        compiler.register_bin_op(BinOp::Sub, |lhs: f64, rhs: f64| lhs - rhs);
        compiler.register_bin_op(BinOp::Mul, |lhs: f64, rhs: f64| lhs * rhs);
        compiler.register_bin_op(BinOp::Div, |lhs: f64, rhs: f64| lhs / rhs);
        compiler.register_un_op(UnOp::Neg, |rhs: f64| -rhs);
        compiler.register_un_op(UnOp::Plus, |rhs: f64| rhs);

        compiler
    }
}

impl<Ctx: ExprContext> Compiler<Ctx> {
    fn register_cast<From: 'static, To: 'static>(
        &mut self,
        cast_fn: fn(From) -> To,
    ) {
        let key = (TypeId::of::<From>(), TypeId::of::<To>());
        let compile_func =
            Box::new(move |from: DynFn| -> Result<DynFn, String> {
                let from = from
                    .downcast::<Ctx, From>()
                    .ok_or("Compiler error: from type mistmatch")?;
                Ok(DynFn::new(move |ctx| cast_fn(from(ctx))))
            });
        self.casts.insert(key, compile_func);
    }

    fn register_un_op<T: 'static>(&mut self, op: UnOp, un_op_fn: fn(T) -> T) {
        let key = (op, TypeId::of::<T>());
        let compile_func =
            Box::new(move |rhs: DynFn| -> Result<DynFn, String> {
                let rhs = rhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: rhs type mistmatch")?;
                Ok(DynFn::new(move |ctx| un_op_fn(rhs(ctx))))
            });
        self.unary_operations.insert(key, compile_func);
    }

    fn register_bin_op<T: 'static>(
        &mut self,
        op: BinOp,
        bin_op_fn: fn(T, T) -> T,
    ) {
        let key = (op, TypeId::of::<T>());
        let compile_func =
            Box::new(move |lhs: DynFn, rhs: DynFn| -> Result<DynFn, String> {
                let lhs = lhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: lhs type mistmatch")?;
                let rhs = rhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: rhs type mistmatch")?;
                Ok(DynFn::new(move |ctx| bin_op_fn(lhs(ctx), rhs(ctx))))
            });
        self.binary_operations.insert(key, compile_func);
    }

    pub fn register_field_access<Obj: 'static, Field: 'static>(
        &mut self,
        field_name: &'static str,
        field_getter: fn(&Obj) -> Field,
    ) {
        let key = (TypeId::of::<Obj>(), field_name);
        let compile_func =
            Box::new(move |obj: DynFn| -> Result<DynFn, String> {
                let obj = obj
                    .downcast::<Ctx, Obj>()
                    .ok_or("Compiler error: obj type mistmatch")?;
                Ok(DynFn::new(move |ctx| field_getter(&obj(ctx))))
            });
        self.field_access.insert(key, compile_func);
    }

    // helper function that tries to cast expression to given type
    fn cast(&self, expr: DynFn, ty: TypeId) -> Result<DynFn, String> {
        if expr.ret_type == ty {
            return Ok(expr);
        }
        let key = (expr.ret_type, ty);
        let Some(compile_cast_func) = self.casts.get(&key) else {
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
                    self.unary_operations.get(&(*op, rhs.ret_type))
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
                    self.binary_operations.get(&(*op, lhs.ret_type))
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
