mod dyn_fn;
mod expr;
mod expr_parser;

use std::{any::TypeId, collections::HashMap, marker::PhantomData};

pub use dyn_fn::*;
pub use expr::*;
pub use expr_parser::*;

pub trait ExprContext: 'static {
    fn field_getter(field_name: &str) -> Option<fn(&Self) -> f64>;
}

type BinOpKey = (BinOp, TypeId);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn, String>>;

type CastKey = (TypeId, TypeId);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

pub struct Compiler<Ctx> {
    casts: HashMap<CastKey, CompileCastFunc>,
    binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    ctx_type: PhantomData<Ctx>,
}

impl<Ctx: ExprContext> Default for Compiler<Ctx> {
    fn default() -> Self {
        let mut compiler = Self {
            casts: HashMap::new(),
            binary_operations: HashMap::new(),
            ctx_type: PhantomData,
        };

        compiler.register_cast(|value: i64| value as f64);

        compiler.register_bin_op(BinOp::Add, |lhs: i64, rhs: i64| lhs + rhs);
        compiler.register_bin_op(BinOp::Sub, |lhs: i64, rhs: i64| lhs - rhs);
        compiler.register_bin_op(BinOp::Mul, |lhs: i64, rhs: i64| lhs * rhs);
        compiler.register_bin_op(BinOp::Div, |lhs: i64, rhs: i64| lhs / rhs);

        compiler.register_bin_op(BinOp::Add, |lhs: f64, rhs: f64| lhs + rhs);
        compiler.register_bin_op(BinOp::Sub, |lhs: f64, rhs: f64| lhs - rhs);
        compiler.register_bin_op(BinOp::Mul, |lhs: f64, rhs: f64| lhs * rhs);
        compiler.register_bin_op(BinOp::Div, |lhs: f64, rhs: f64| lhs / rhs);

        compiler
    }
}

impl<Ctx: ExprContext> Compiler<Ctx> {
    fn register_cast<From: 'static, To: 'static>(&mut self, cast_fn: fn(From) -> To) {
        let key = (TypeId::of::<From>(), TypeId::of::<To>());
        let compile_func = Box::new(move |from: DynFn| -> Result<DynFn, String> {
            let from = from
                .downcast::<Ctx, From>()
                .ok_or("Compiler error: from type mistmatch")?;
            Ok(DynFn::new(move |ctx| cast_fn(from(ctx))))
        });
        self.casts.insert(key, compile_func);
    }

    fn register_bin_op<T: 'static>(&mut self, op: BinOp, bin_op_fn: fn(T, T) -> T) {
        let key = (op, TypeId::of::<T>());
        let compile_func = Box::new(move |lhs: DynFn, rhs: DynFn| -> Result<DynFn, String> {
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

    fn cast_same_type(&self, a: DynFn, b: DynFn) -> Result<(DynFn, DynFn), String> {
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
            Expr::Var(var_name) => {
                let field_getter =
                    Ctx::field_getter(var_name).ok_or(format!("Unknown variable ${var_name}"))?;
                DynFn::new(field_getter)
            }
            Expr::UnOp(op, rhs) => {
                let rhs_dyn = self.compile_expr(rhs)?;
                if rhs_dyn.ret_type == TypeId::of::<f64>() {
                    let rhs = rhs_dyn
                        .downcast::<Ctx, f64>()
                        .ok_or("Compiler error: rhs type mismatch")?;
                    match op {
                        UnOp::Neg => DynFn::new(move |ctx| -rhs(ctx)),
                        UnOp::Plus => rhs_dyn,
                    }
                } else {
                    Err("Unsupported unary operation")?
                }
            }
            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_expr(lhs)?;
                let rhs = self.compile_expr(rhs)?;

                let (lhs, rhs) = self.cast_same_type(lhs, rhs)?;

                let Some(compile_bin_op) = self.binary_operations.get(&(*op, lhs.ret_type)) else {
                    Err("Unsupported binary operation")?
                };

                compile_bin_op(lhs, rhs)?
            }
            Expr::FieldAccess(_object, _field_name) => Err("Field access not supported")?,
            Expr::FuncCall(_function, _arguments) => Err("Function calls not supported")?,
        })
    }

    pub fn compile<Ret: 'static>(&self, expr: &Expr) -> Result<BoxedFn<Ctx, Ret>, String> {
        let dyn_fn = self.compile_expr(expr)?;
        let casted_dyn_fn = self.cast(dyn_fn, TypeId::of::<Ret>())?;
        casted_dyn_fn
            .downcast::<Ctx, Ret>()
            .ok_or("Compiler error: type mismatch".to_string())
    }
}

pub fn eval<Ctx: ExprContext>(input: &str, ctx: &Ctx) -> Result<f64, String> {
    let expr = parse_expr(input);
    if expr.has_errors() || !expr.has_output() {
        let errors = expr
            .errors()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(",");
        Err(format!("Error parsing expression: {}", errors))?;
    }

    let compiler = Compiler::<Ctx>::default();
    let compiled_expr = compiler.compile::<f64>(expr.output().unwrap())?;
    Ok(compiled_expr(ctx))
}

#[cfg(test)]
mod tests {
    use crate::*;

    struct TestContext {
        foo: f64,
        bar: f64,
    }

    impl ExprContext for TestContext {
        fn field_getter(field_name: &str) -> Option<fn(&Self) -> f64> {
            match field_name {
                "foo" => Some(|ctx: &TestContext| ctx.foo),
                "bar" => Some(|ctx: &TestContext| ctx.bar),
                _ => None,
            }
        }
    }

    #[test]
    fn test_eval() {
        let ctx = TestContext { foo: 1.0, bar: 2.5 };
        assert_eq!(eval("(1 + 2) * 3", &ctx), Ok((1.0 + 2.0) * 3.0));
        assert_eq!(eval("2 * (foo + bar)", &ctx), Ok(2.0 * (ctx.foo + ctx.bar)));
    }

    #[test]
    fn test_dyn_fn() {
        // here we construct a function that takes an (i32,i32) tuple and returns the first part
        // but the type of the variable just DynFn, no mention of tuples and i32
        let dyn_fn = DynFn::new(|a: &(i32, i32)| a.0);

        // here we get back to the callable function with known types
        // but for that we need to know exact types at compile time
        let concrete_fn = dyn_fn.downcast::<(i32, i32), i32>().unwrap();

        // and here we call the downcasted function to test if it really works as intended
        assert_eq!((concrete_fn)(&(10, 20)), 10);
    }
}
