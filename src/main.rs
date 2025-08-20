use crate::{
    boxed_fn::{BoxedFnRetVal, DynBoxedFn},
    expr::{BinOp, Expr, UnOp},
    expr_parser::parse_expr,
    tdesc::TDesc,
};
use std::{collections::HashMap, marker::PhantomData};

pub mod boxed_fn;
pub mod expr;
pub mod expr_parser;
pub mod tdesc;

#[derive(Clone)]
pub struct CompiledExpr {
    dyn_fn: DynBoxedFn,
    arg_type: TDesc,
    ret_type: TDesc,
    ret_ref: bool,
}

impl CompiledExpr {
    fn downcast<Arg, Ret>(&self) -> CompilerResult<BoxedFnRetVal<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        let (arg_type, ret_type) = (TDesc::of::<Arg>(), TDesc::of::<Ret>());
        if self.arg_type != arg_type || self.ret_type != ret_type || self.ret_ref {
            return Err(format!(
                "Failed CompiledExpr downcast. Expected Arg:{:?} Ret:{:?} returns_ref:false. Got Arg:{:?}, Ret{:?} returns_ref:{:?}",
                arg_type, ret_type, self.arg_type, self.ret_type, self.ret_ref
            ));
        }

        let typed_fn = self.dyn_fn.downcast_ret_val::<Arg, Ret>().unwrap();

        Ok(typed_fn)
    }

    fn make<Arg, Ret>(f: impl Fn(&Arg) -> Ret + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            dyn_fn: DynBoxedFn::make_ret_val(f),
            arg_type: TDesc::of::<Arg>(),
            ret_type: TDesc::of::<Ret>(),
            ret_ref: false,
        }
    }
}

// returned CompiledExpr must have arg_type = Object
type ObjectFields = HashMap<&'static str, CompiledExpr>;

pub trait Object: 'static {
    fn fields() -> Option<ObjectFields>;
}

impl Object for () {
    fn fields() -> Option<ObjectFields> {
        None
    }
}

type CompilerResult<T> = Result<T, String>;

type CastFn = Box<dyn Fn(&CompiledExpr) -> CompilerResult<CompiledExpr>>;
type UnopFn = Box<dyn Fn(CompiledExpr) -> CompilerResult<CompiledExpr>>;
type BinopFn = Box<dyn Fn(CompiledExpr, CompiledExpr) -> CompilerResult<CompiledExpr>>;

pub struct Compiler<Arg> {
    casts: HashMap<(TDesc, TDesc), CastFn>,
    un_ops: HashMap<(TDesc, UnOp), UnopFn>,
    bin_ops: HashMap<(TDesc, BinOp), BinopFn>,
    objects: HashMap<TDesc, ObjectFields>,
    phantom_data: PhantomData<Arg>,
}

impl<Arg: Object> Compiler<Arg>
where
    Arg: 'static,
{
    pub fn new() -> Self {
        Self {
            casts: HashMap::new(),
            un_ops: HashMap::new(),
            bin_ops: HashMap::new(),
            objects: HashMap::new(),
            phantom_data: PhantomData,
        }
        .register_cast(|i: i64| i as f64)
        .register_un_op(UnOp::Neg, |rhs: i64| -rhs)
        .register_un_op(UnOp::Neg, |rhs: f64| -rhs)
        .register_un_op(UnOp::Plus, |rhs: i64| rhs)
        .register_un_op(UnOp::Plus, |rhs: f64| rhs)
        .register_bin_op(BinOp::Add, |lhs: i64, rhs| lhs + rhs)
        .register_bin_op(BinOp::Add, |lhs: f64, rhs| lhs + rhs)
        .register_bin_op(BinOp::Sub, |lhs: i64, rhs| lhs - rhs)
        .register_bin_op(BinOp::Sub, |lhs: f64, rhs| lhs - rhs)
        .register_bin_op(BinOp::Mul, |lhs: i64, rhs| lhs * rhs)
        .register_bin_op(BinOp::Mul, |lhs: f64, rhs| lhs * rhs)
        .register_bin_op(BinOp::Div, |lhs: i64, rhs| lhs / rhs)
        .register_bin_op(BinOp::Div, |lhs: f64, rhs| lhs / rhs)
        .register_object::<Arg>()
    }

    fn register_cast<From, To>(mut self, cast_fn: impl Fn(From) -> To + Copy + 'static) -> Self
    where
        From: 'static,
        To: 'static,
    {
        self.casts.insert(
            (TDesc::of::<From>(), TDesc::of::<To>()),
            Box::new(move |from_expr| {
                let from_fn = from_expr.downcast::<Arg, From>()?;
                Ok(CompiledExpr::make(move |arg: &Arg| cast_fn(from_fn(arg))))
            }),
        );
        self
    }

    fn register_un_op<T>(mut self, un_op: UnOp, unop_fn: impl Fn(T) -> T + Copy + 'static) -> Self
    where
        T: 'static,
    {
        self.un_ops.insert(
            (TDesc::of::<T>(), un_op),
            Box::new(move |from_expr| {
                let from_fn = from_expr.downcast::<Arg, T>()?;
                Ok(CompiledExpr::make(move |arg: &Arg| unop_fn(from_fn(arg))))
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
            (TDesc::of::<T>(), bin_op),
            Box::new(
                move |lhs: CompiledExpr, rhs: CompiledExpr| -> CompilerResult<CompiledExpr> {
                    let lhs_fn = lhs.downcast::<Arg, T>()?;
                    let rhs_fn = rhs.downcast::<Arg, T>()?;
                    Ok(CompiledExpr::make(move |arg: &Arg| {
                        bin_op_fn(lhs_fn(arg), rhs_fn(arg))
                    }))
                },
            ),
        );
        self
    }

    fn register_object<O: Object>(mut self) -> Self {
        let ty = TDesc::of::<O>();
        if self.objects.contains_key(&ty) {
            return self;
        }
        let Some(fields) = O::fields() else {
            return self;
        };
        self.objects.insert(ty, fields);
        self
    }

    fn cast<To>(&self, compiled_expr: CompiledExpr) -> CompilerResult<CompiledExpr>
    where
        To: 'static,
    {
        self.cast_to(TDesc::of::<To>(), compiled_expr)
    }

    fn cast_to(&self, to_type: TDesc, compiled_expr: CompiledExpr) -> CompilerResult<CompiledExpr> {
        let from_type = compiled_expr.ret_type;
        if compiled_expr.ret_type == to_type {
            return Ok(compiled_expr.clone());
        }
        let Some(cast_fn) = self.casts.get(&(compiled_expr.ret_type, to_type)) else {
            return Err(format!("Cannot cast {from_type:?} to {to_type:?}"));
        };
        cast_fn(&compiled_expr)
    }

    fn cast_same_type(
        &self,
        a: CompiledExpr,
        b: CompiledExpr,
    ) -> CompilerResult<(CompiledExpr, CompiledExpr)> {
        if a.ret_type == b.ret_type {
            return Ok((a, b));
        }

        if let Ok(b_casted) = self.cast_to(a.ret_type, b.clone()) {
            return Ok((a, b_casted));
        }

        if let Ok(a_casted) = self.cast_to(b.ret_type, a.clone()) {
            return Ok((a_casted, b));
        }

        Err(format!(
            "Cannot cast to same type {:?} and {:?}",
            a.ret_type, b.ret_type
        ))
    }

    pub fn compile_typed(&self, expr: &Expr) -> CompilerResult<CompiledExpr> {
        Ok(match expr {
            &Expr::Int(val) => CompiledExpr::make(move |_: &Arg| val),

            &Expr::Float(val) => CompiledExpr::make(move |_: &Arg| val),

            Expr::Var(name) => {
                let arg_type = TDesc::of::<Arg>();
                let Some(field_getter) = self
                    .objects
                    .get(&arg_type)
                    .and_then(|fields| fields.get(name.as_str()))
                else {
                    return Err(format!("No field {name} on type {arg_type}"));
                };
                field_getter.clone()
            }

            Expr::UnOp(op, rhs) => {
                let rhs = self.compile_typed(rhs)?;
                let Some(un_op_fn) = self.un_ops.get(&(rhs.ret_type, *op)) else {
                    return Err(format!(
                        "No unary operator {:?} for type {:?}",
                        op, rhs.ret_type
                    ));
                };
                un_op_fn(rhs)?
            }

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
        })
    }

    pub fn compile<Ret>(&self, expr: &Expr) -> CompilerResult<BoxedFnRetVal<Arg, Ret>>
    where
        Ret: 'static,
    {
        let compiled_expr = self.compile_typed(expr)?;
        self.cast::<Ret>(compiled_expr)?.downcast()
    }
}

impl<Arg: Object> Default for Compiler<Arg>
where
    Arg: 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

struct Foo {
    a: i64,
    b: f64,
}

impl Object for Foo {
    fn fields() -> Option<ObjectFields> {
        Some(
            [
                ("a", CompiledExpr::make(|obj: &Foo| obj.a)),
                ("b", CompiledExpr::make(|obj: &Foo| obj.b)),
            ]
            .into(),
        )
    }
}

fn main() {
    let src = "-+-(30+10 * +a) / -b";
    let parsed = parse_expr(src).unwrap();
    let compiler = Compiler::<Foo>::new();
    let compiled = compiler.compile::<f64>(&parsed).unwrap();
    let ctx = Foo { a: 10, b: 20.5 };
    let result = (compiled)(&ctx);
    println!("{result:?}");
}
