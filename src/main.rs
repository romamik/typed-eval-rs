use crate::{
    boxed_fn::{BoxedFnRetRef, BoxedFnRetVal, DynBoxedFn},
    expr::{BinOp, Expr, UnOp},
    expr_parser::parse_expr,
    tdesc::TDesc,
};
use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

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

    fn downcast_ret_ref<Arg, Ret>(&self) -> CompilerResult<BoxedFnRetRef<Arg, Ret>>
    where
        Arg: 'static,
        Ret: 'static,
    {
        let (arg_type, ret_type) = (TDesc::of::<Arg>(), TDesc::of::<Ret>());
        if self.arg_type != arg_type || self.ret_type != ret_type || !self.ret_ref {
            return Err(format!(
                "Failed CompiledExpr downcast. Expected Arg:{:?} Ret:{:?} returns_ref:true. Got Arg:{:?}, Ret{:?} returns_ref:{:?}",
                arg_type, ret_type, self.arg_type, self.ret_type, self.ret_ref
            ));
        }

        let typed_fn = self.dyn_fn.downcast_ret_ref::<Arg, Ret>().unwrap();

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

    fn make_ret_ref<Arg, Ret>(f: impl Fn(&Arg) -> &Ret + 'static) -> Self
    where
        Arg: 'static,
        Ret: 'static,
    {
        Self {
            dyn_fn: DynBoxedFn::make_ret_ref(f),
            arg_type: TDesc::of::<Arg>(),
            ret_type: TDesc::of::<Ret>(),
            ret_ref: true,
        }
    }
}

pub trait SupportedType: Sized + 'static {
    // register type with compiler, call all these register_bin_op, register_field
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>);

    // create a CompiledExpr that takes Arg of type Obj and returns a field
    // function always take a function that returns a reference to a field
    // but can return CompiledExpr that returns either reference or a value
    // for primitive types it should be value, as operators and casts only work with values
    // for objects it should be reference to avoid cloning objects
    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + 'static) -> CompiledExpr;
}

impl SupportedType for i64 {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_cast(|i: i64| i as f64)
            .register_un_op(UnOp::Neg, |i: i64| -i)
            .register_un_op(UnOp::Plus, |i: i64| i)
            .register_bin_op(BinOp::Add, |a: i64, b| a + b)
            .register_bin_op(BinOp::Sub, |a: i64, b| a - b)
            .register_bin_op(BinOp::Mul, |a: i64, b| a * b)
            .register_bin_op(BinOp::Div, |a: i64, b| a / b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + 'static) -> CompiledExpr {
        CompiledExpr::make(move |obj: &Obj| *getter(obj))
    }
}

impl SupportedType for f64 {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_un_op(UnOp::Neg, |i: f64| -i)
            .register_un_op(UnOp::Plus, |i: f64| i)
            .register_bin_op(BinOp::Add, |a: f64, b| a + b)
            .register_bin_op(BinOp::Sub, |a: f64, b| a - b)
            .register_bin_op(BinOp::Mul, |a: f64, b| a * b)
            .register_bin_op(BinOp::Div, |a: f64, b| a / b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + 'static) -> CompiledExpr {
        CompiledExpr::make(move |obj: &Obj| *getter(obj))
    }
}

type CompilerResult<T> = Result<T, String>;

type CastFn = Box<dyn Fn(&CompiledExpr) -> CompilerResult<CompiledExpr>>;
type UnopFn = Box<dyn Fn(CompiledExpr) -> CompilerResult<CompiledExpr>>;
type BinopFn = Box<dyn Fn(CompiledExpr, CompiledExpr) -> CompilerResult<CompiledExpr>>;
type FieldFn = Box<dyn Fn(CompiledExpr) -> CompilerResult<CompiledExpr>>;

pub struct Compiler<Arg> {
    registered_types: HashSet<TDesc>,
    casts: HashMap<(TDesc, TDesc), CastFn>,
    un_ops: HashMap<(TDesc, UnOp), UnopFn>,
    bin_ops: HashMap<(TDesc, BinOp), BinopFn>,
    fields: HashMap<(TDesc, &'static str), FieldFn>,
    phantom_data: PhantomData<Arg>,
}

impl<Arg> Compiler<Arg>
where
    Arg: SupportedType,
{
    pub fn new() -> Self {
        let mut compiler = Self {
            registered_types: HashSet::new(),
            casts: HashMap::new(),
            un_ops: HashMap::new(),
            bin_ops: HashMap::new(),
            fields: HashMap::new(),
            phantom_data: PhantomData,
        };
        compiler.register_type::<Arg>();
        compiler
    }

    pub fn register_type<T: SupportedType + 'static>(&mut self) {
        let ty = TDesc::of::<T>();
        if self.registered_types.contains(&ty) {
            return;
        }
        self.registered_types.insert(ty);
        T::register(self);
    }

    pub fn register_cast<From, To>(
        &mut self,
        cast_fn: impl Fn(From) -> To + Copy + 'static,
    ) -> &mut Self
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

    pub fn register_un_op<T>(
        &mut self,
        un_op: UnOp,
        unop_fn: impl Fn(T) -> T + Copy + 'static,
    ) -> &mut Self
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

    pub fn register_bin_op<T>(
        &mut self,
        bin_op: BinOp,
        bin_op_fn: impl Fn(T, T) -> T + Copy + 'static,
    ) -> &mut Self
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

    pub fn register_field<Obj, Field>(
        &mut self,
        field_name: &'static str,
        getter: impl Fn(&Obj) -> &Field + Copy + 'static,
    ) -> &mut Self
    where
        Obj: 'static,
        Field: SupportedType,
    {
        self.register_type::<Field>();

        let obj_type = TDesc::of::<Obj>();
        self.fields.insert(
            (obj_type, field_name),
            Box::new(move |obj: CompiledExpr| -> CompilerResult<CompiledExpr> {
                let obj_fn = obj.downcast_ret_ref::<Arg, Obj>()?;
                Ok(Field::make_getter(move |arg: &Arg| getter(obj_fn(arg))))
            }),
        );
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
                let Some(field_fn) = self.fields.get(&(arg_type, name)) else {
                    return Err(format!("No field {name} on type {arg_type}"));
                };
                field_fn(CompiledExpr::make_ret_ref(|arg: &Arg| arg))?
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

            Expr::FieldAccess(obj, field_name) => {
                let obj = self.compile_typed(obj)?;
                let obj_type = obj.ret_type;
                if !obj.ret_ref {
                    return Err("Invalid field access: need reference type".to_string());
                }
                let Some(field_fn) = self.fields.get(&(obj_type, field_name)) else {
                    return Err(format!("No field {field_name} on type {obj_type}"));
                };

                field_fn(obj)?
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

impl<Arg> Default for Compiler<Arg>
where
    Arg: SupportedType,
{
    fn default() -> Self {
        Self::new()
    }
}

struct Foo {
    a: i64,
    b: f64,
}

impl SupportedType for Foo {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_field("a", |obj: &Self| &obj.a)
            .register_field("b", |obj: &Self| &obj.b);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + 'static) -> CompiledExpr {
        CompiledExpr::make_ret_ref(getter)
    }
}

struct Bar {
    c: i64,
    foo: Foo,
}

impl SupportedType for Bar {
    fn register<Arg: SupportedType>(compiler: &mut Compiler<Arg>) {
        compiler
            .register_field("c", |obj: &Self| &obj.c)
            .register_field("foo", |obj: &Self| &obj.foo);
    }

    fn make_getter<Obj: 'static>(getter: impl Fn(&Obj) -> &Self + 'static) -> CompiledExpr {
        CompiledExpr::make_ret_ref(getter)
    }
}

fn main() {
    let src = "  (foo.a*foo.b - c) * +-+-2.0 / --2 ";
    let parsed = parse_expr(src).unwrap();
    let compiler = Compiler::<Bar>::new();
    let compiled = compiler.compile::<f64>(&parsed).unwrap();
    let ctx = Bar {
        foo: Foo { a: 10, b: 20.5 },
        c: 20,
    };
    let result = (compiled)(&ctx);
    println!("{result:?}");
}
