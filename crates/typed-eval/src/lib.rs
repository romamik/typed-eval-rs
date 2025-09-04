mod expr;
mod expr_parser;

use std::{
    any::{Any, TypeId},
    ops::Deref,
};

pub use expr::*;
pub use expr_parser::*;

// the function with a statically known type
pub struct BoxedFn<Arg, Ret>(Box<dyn ClonableFn<Arg, Ret>>);

impl<Arg, Ret> Clone for BoxedFn<Arg, Ret> {
    fn clone(&self) -> Self {
        self.clone_boxed()
    }
}

// implementing Deref allows to use the call syntax on the BoxedFn instances
impl<Arg, Ret> Deref for BoxedFn<Arg, Ret> {
    type Target = Box<dyn ClonableFn<Arg, Ret>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait ClonableFn<Arg, Ret>: Fn(&Arg) -> Ret {
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret>;
}

// implement ClonableFn for every matching function
impl<Arg, Ret, F> ClonableFn<Arg, Ret> for F
where
    F: Fn(&Arg) -> Ret + Clone + 'static,
{
    fn clone_boxed(&self) -> BoxedFn<Arg, Ret> {
        BoxedFn(Box::new(self.clone()))
    }
}

// the function with a dynamically known type
pub struct DynFn {
    boxed_fun: Box<dyn Any>,
    arg_type: TypeId,
    ret_type: TypeId,
}

impl DynFn {
    pub fn new<Arg, Ret>(f: impl Fn(&Arg) -> Ret + Clone + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            boxed_fun: Box::new(BoxedFn(Box::new(f))),
            arg_type: TypeId::of::<Arg>(),
            ret_type: TypeId::of::<Ret>(),
        }
    }

    pub fn downcast<Arg, Ret>(&self) -> Option<BoxedFn<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.boxed_fun.downcast_ref().cloned()
    }
}

pub trait ExprContext: 'static {
    fn field_getter(field_name: &str) -> Option<fn(&Self) -> f64>;
}

pub fn compile_expr<Ctx: ExprContext>(expr: &Expr) -> Result<DynFn, String> {
    Ok(match expr {
        &Expr::Int(val) => DynFn::new(move |_ctx: &Ctx| val as f64),
        &Expr::Float(val) => DynFn::new(move |_ctx: &Ctx| val),
        Expr::String(_string) => Err("Strings not supported")?,
        Expr::Var(var_name) => {
            let field_getter =
                Ctx::field_getter(var_name).ok_or(format!("Unknown variable ${var_name}"))?;
            DynFn::new(field_getter)
        }
        Expr::UnOp(op, rhs) => {
            let rhs_dyn = compile_expr::<Ctx>(rhs)?;
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
            let lhs = compile_expr::<Ctx>(lhs)?;
            let rhs = compile_expr::<Ctx>(rhs)?;
            if lhs.ret_type == TypeId::of::<f64>() && rhs.ret_type == TypeId::of::<f64>() {
                let lhs = lhs
                    .downcast::<Ctx, f64>()
                    .ok_or("Compiler error: lhs type mismatch")?;
                let rhs = rhs
                    .downcast::<Ctx, f64>()
                    .ok_or("Compiler error: rhs type mismatch")?;

                match op {
                    BinOp::Add => DynFn::new(move |ctx| lhs(ctx) + rhs(ctx)),
                    BinOp::Sub => DynFn::new(move |ctx| lhs(ctx) - rhs(ctx)),
                    BinOp::Mul => DynFn::new(move |ctx| lhs(ctx) * rhs(ctx)),
                    BinOp::Div => DynFn::new(move |ctx| lhs(ctx) / rhs(ctx)),
                }
            } else {
                Err("Unsupported binary operation")?
            }
        }
        Expr::FieldAccess(_object, _field_name) => Err("Field access not supported")?,
        Expr::FuncCall(_function, _arguments) => Err("Function calls not supported")?,
    })
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

    let compiled_expr = compile_expr::<Ctx>(expr.output().unwrap())?
        .downcast::<Ctx, f64>()
        .ok_or("compiled function was of wrong type")?;
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
