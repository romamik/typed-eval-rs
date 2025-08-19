use crate::{
    expr::{BinOp, Expr},
    expr_parser::parse_expr,
};
use std::{
    any::{Any, TypeId, type_name},
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
};

pub mod expr;
pub mod expr_parser;

#[derive(Clone, Copy)]
pub struct TypeDescr {
    type_id: TypeId,
    type_name: &'static str,
}

impl TypeDescr {
    pub fn of<T>() -> Self
    where
        T: 'static,
    {
        Self {
            type_id: TypeId::of::<T>(),
            type_name: type_name::<T>(),
        }
    }
}

impl PartialEq for TypeDescr {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

impl Eq for TypeDescr {}

impl Hash for TypeDescr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_id.hash(state);
    }
}

impl std::fmt::Debug for TypeDescr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

impl std::fmt::Display for TypeDescr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

struct TypedFn<Arg, Ret>(Box<dyn Fn(&Arg) -> Ret>);

pub struct CompiledExpr {
    dyn_fn: Box<dyn Any>,
    arg_type: TypeDescr,
    ret_type: TypeDescr,
}

impl CompiledExpr {
    fn downcast<Arg, Ret>(self) -> Result<impl Fn(&Arg) -> Ret, Self>
    where
        Arg: 'static,
        Ret: 'static,
    {
        let (arg_type, ret_type) = (TypeDescr::of::<Arg>(), TypeDescr::of::<Ret>());
        if self.arg_type != arg_type || self.ret_type != ret_type {
            return Err(self);
        }
        let typed_fn = self
            .dyn_fn
            .downcast::<TypedFn<Arg, Ret>>()
            .map_err(|dyn_fn| Self { dyn_fn, ..self })?;

        Ok(typed_fn.0)
    }

    fn try_downcast<Arg, Ret>(self) -> CompilerResult<impl Fn(&Arg) -> Ret>
    where
        Arg: 'static,
        Ret: 'static,
    {
        self.downcast().map_err(|compiled_expr| {
            format!(
                "Failed downcast. Expected Arg:{:?} Ret:{:?}. Got Arg:{:?}, Ret{:?}",
                TypeId::of::<Arg>(),
                TypeId::of::<Ret>(),
                compiled_expr.arg_type,
                compiled_expr.ret_type
            )
        })
    }

    fn make<Arg, Ret>(f: impl Fn(&Arg) -> Ret + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            dyn_fn: Box::new(TypedFn(Box::new(f))),
            arg_type: TypeDescr::of::<Arg>(),
            ret_type: TypeDescr::of::<Ret>(),
        }
    }
}

type CompilerResult<T> = Result<T, String>;

struct Compiler<Arg> {
    casts: HashMap<
        (TypeDescr, TypeDescr),
        Box<dyn Fn(CompiledExpr) -> Result<CompiledExpr, CompiledExpr>>,
    >,
    bin_ops: HashMap<
        (TypeDescr, BinOp),
        Box<dyn Fn(CompiledExpr, CompiledExpr) -> CompilerResult<CompiledExpr>>,
    >,
    phantom_data: PhantomData<Arg>,
}

impl<Arg> Compiler<Arg>
where
    Arg: 'static,
{
    pub fn new() -> Self {
        Self {
            casts: HashMap::new(),
            bin_ops: HashMap::new(),
            phantom_data: PhantomData,
        }
        .register_cast(|i: i64| i as f64)
        .register_bin_op(BinOp::Add, |lhs: i64, rhs| lhs + rhs)
        .register_bin_op(BinOp::Add, |lhs: f64, rhs| lhs + rhs)
    }

    fn register_cast<From, To>(mut self, cast_fn: impl Fn(From) -> To + Copy + 'static) -> Self
    where
        From: 'static,
        To: 'static,
    {
        self.casts.insert(
            (TypeDescr::of::<From>(), TypeDescr::of::<To>()),
            Box::new(move |from_expr| {
                let from_fn = from_expr.downcast::<Arg, From>()?;
                Ok(CompiledExpr::make(move |arg: &Arg| cast_fn(from_fn(arg))))
            }),
        );
        self
    }

    fn register_bin_op<T>(
        mut self,
        bin_op: BinOp,
        bin_op_fn: impl Fn(T, T) -> T + Copy + 'static,
    ) -> Self
    where
        T: 'static,
    {
        self.bin_ops.insert(
            (TypeDescr::of::<T>(), bin_op),
            Box::new(
                move |lhs: CompiledExpr, rhs: CompiledExpr| -> CompilerResult<CompiledExpr> {
                    let lhs_fn = lhs.try_downcast::<Arg, T>()?;
                    let rhs_fn = rhs.try_downcast::<Arg, T>()?;
                    Ok(CompiledExpr::make(move |arg: &Arg| {
                        bin_op_fn(lhs_fn(arg), rhs_fn(arg))
                    }))
                },
            ),
        );
        self
    }

    fn cast<To>(&self, compiled_expr: CompiledExpr) -> CompilerResult<CompiledExpr>
    where
        To: 'static,
    {
        let from_type = compiled_expr.ret_type;
        let to_type = TypeDescr::of::<To>();
        self.cast_to(to_type, compiled_expr)
            .map_err(|_| format!("Cannot cast {from_type:?} to {to_type:?}"))
    }

    fn cast_to(
        &self,
        to: TypeDescr,
        compiled_expr: CompiledExpr,
    ) -> Result<CompiledExpr, CompiledExpr> {
        if compiled_expr.ret_type == to {
            return Ok(compiled_expr);
        }
        let Some(cast_fn) = self.casts.get(&(compiled_expr.ret_type, to)) else {
            return Err(compiled_expr);
        };
        cast_fn(compiled_expr)
    }

    fn cast_same_type(
        &self,
        a: CompiledExpr,
        b: CompiledExpr,
    ) -> CompilerResult<(CompiledExpr, CompiledExpr)> {
        if a.ret_type == b.ret_type {
            return Ok((a, b));
        }

        let b = match self.cast_to(a.ret_type, b) {
            Ok(b_casted) => return Ok((a, b_casted)),
            Err(b_failed) => b_failed,
        };

        let a = match self.cast_to(b.ret_type, a) {
            Ok(a_casted) => return Ok((a_casted, b)),
            Err(a_failed) => a_failed,
        };

        Err(format!(
            "Cannot cast to same type {:?} and {:?}",
            a.ret_type, b.ret_type
        ))
    }

    pub fn compile_typed(&self, expr: &Expr) -> CompilerResult<CompiledExpr> {
        Ok(match expr {
            &Expr::Int(val) => CompiledExpr::make(move |_: &Arg| val),
            &Expr::Float(val) => CompiledExpr::make(move |_: &Arg| val),
            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_typed(lhs)?;
                let rhs = self.compile_typed(rhs)?;
                let (lhs, rhs) = self.cast_same_type(lhs, rhs)?;
                let Some(bin_op_fn) = self.bin_ops.get(&(lhs.ret_type, *op)) else {
                    return Err(format!(
                        "No binary operator {:?} for type {:?}",
                        op, lhs.ret_type
                    ));
                };
                bin_op_fn(lhs, rhs)?
            }
            _ => todo!(),
        })
    }

    pub fn compile<Ret>(&self, expr: &Expr) -> CompilerResult<impl Fn(&Arg) -> Ret>
    where
        Ret: 'static,
    {
        let compiled_expr = self.compile_typed(expr)?;
        self.cast::<Ret>(compiled_expr)?.try_downcast()
    }
}

fn main() {
    let src = "30e1 + 10"; //"300000000 + 4 *(2- x) / 7.5 ";
    let parsed = parse_expr(src).unwrap();

    let compiler = Compiler::<()>::new();

    let compiled = compiler.compile::<i64>(&parsed).unwrap();
    let result = (compiled)(&());
    println!("{result:?}");
}
