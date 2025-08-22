use crate::{
    BinOp, BoxedFnRetVal, DynBoxedFn, Expr, RetType, SupportedType, TDesc, UnOp, parse_expr,
    register_basic_types,
};

use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

pub struct Compiler<Ctx> {
    inner: CompilerInner<Ctx>,
}

pub struct CompilerInner<Ctx> {
    registered_types: HashSet<TDesc>,
    casts: HashMap<(RetType, RetType), CastFn>,
    un_ops: HashMap<(RetType, UnOp), UnopFn>,
    bin_ops: HashMap<(RetType, BinOp), BinopFn>,
    fields: HashMap<(RetType, &'static str), FieldFn>,
    functions: HashMap<RetType, FunctionFn<Ctx>>, // key is type of the function
    phantom_data: PhantomData<Ctx>,
}

pub type CompilerResult<T> = Result<T, String>;

type CastFn = Box<dyn Fn(&DynBoxedFn) -> Option<DynBoxedFn>>;
type UnopFn = Box<dyn Fn(DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
type BinopFn = Box<dyn Fn(DynBoxedFn, DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
type FieldFn = Box<dyn Fn(DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
type FunctionFn<Ctx> =
    Box<dyn Fn(&Compiler<Ctx>, DynBoxedFn, Vec<DynBoxedFn>) -> CompilerResult<DynBoxedFn>>;

impl<Ctx> Compiler<Ctx>
where
    Ctx: SupportedType,
{
    pub fn new() -> Self {
        let mut inner = CompilerInner {
            registered_types: Default::default(),
            casts: Default::default(),
            un_ops: Default::default(),
            bin_ops: Default::default(),
            fields: Default::default(),
            functions: Default::default(),
            phantom_data: PhantomData,
        };
        register_basic_types(&mut inner);
        inner.register_type::<Ctx>();
        Self { inner }
    }

    // compile AST to a function with a known return type
    // can result in cast error if expression resolves to incompatible type
    pub fn compile<Ret>(&self, src: &str) -> CompilerResult<BoxedFnRetVal<Ctx, Ret>>
    where
        Ret: 'static,
    {
        let parse_result = parse_expr(src);
        let expr = parse_result.output().ok_or_else(|| {
            "Parsing failed:".to_string()
                + &parse_result
                    .errors()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n")
        })?;

        let compiled_expr = self.compile_typed(expr)?;
        let casted_fn = self.cast::<Ret>(compiled_expr)?.get_fn_ret_val()?;
        Ok(casted_fn)
    }

    // compile AST to a function with a known return type
    // can result in cast error if expression resolves to incompatible type
    pub fn compile_expr<Ret>(&self, expr: &Expr) -> CompilerResult<BoxedFnRetVal<Ctx, Ret>>
    where
        Ret: 'static,
    {
        let compiled_expr = self.compile_typed(expr)?;
        let casted_fn = self.cast::<Ret>(compiled_expr)?.get_fn_ret_val()?;
        Ok(casted_fn)
    }

    // compile AST to DynBoxedFn, the return type is determined from expression
    pub fn compile_typed(&self, expr: &Expr) -> CompilerResult<DynBoxedFn> {
        Ok(match expr {
            &Expr::Int(val) => DynBoxedFn::make_ret_val(move |_: &Ctx| val),

            &Expr::Float(val) => DynBoxedFn::make_ret_val(move |_: &Ctx| val),

            Expr::String(str) => {
                let str = str.clone();
                DynBoxedFn::make_ret_val(move |_: &Ctx| str.clone())
            }

            Expr::Var(name) => {
                self.compile_field_access(DynBoxedFn::make_ret_ref(|ctx: &Ctx| ctx), name)?
            }

            Expr::UnOp(op, rhs) => {
                let rhs = self.compile_typed(rhs)?;
                let Some(compile_un_op_fn) = self.inner.un_ops.get(&(rhs.ret_type, *op)) else {
                    return Err(format!(
                        "No unary operator {:?} for type {:?}",
                        op, rhs.ret_type
                    ));
                };
                compile_un_op_fn(rhs)?
            }

            Expr::BinOp(op, lhs, rhs) => {
                let lhs = self.compile_typed(lhs)?;
                let rhs = self.compile_typed(rhs)?;
                let (lhs, rhs) = self.cast_same_type(lhs, rhs)?;
                let Some(compile_bin_op_fn) = self.inner.bin_ops.get(&(lhs.ret_type, *op)) else {
                    return Err(format!(
                        "No binary operator {:?} for type {:?}",
                        op, lhs.ret_type
                    ));
                };
                compile_bin_op_fn(lhs, rhs)?
            }

            Expr::FieldAccess(obj, field_name) => {
                let obj = self.compile_typed(obj)?;
                self.compile_field_access(obj, field_name)?
            }

            Expr::FuncCall(func, args) => {
                let func = self.compile_typed(func)?;
                let args = args
                    .iter()
                    .map(|arg| self.compile_typed(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let Some(compile_func_fn) = self.inner.functions.get(&func.ret_type) else {
                    return Err(format!("Cannot call {:?}", func.ret_type));
                };

                compile_func_fn(self, func, args)?
            }
        })
    }

    fn compile_field_access(
        &self,
        obj: DynBoxedFn,
        field_name: &str,
    ) -> CompilerResult<DynBoxedFn> {
        let obj_type = obj.ret_type;
        let Some(compile_field_access_fn) = self.inner.fields.get(&(obj_type, field_name)) else {
            return Err(format!("No field {field_name} on type {obj_type}"));
        };

        compile_field_access_fn(obj)
    }

    // try cast expression so that it returns type To
    pub fn cast<To>(&self, from: DynBoxedFn) -> CompilerResult<DynBoxedFn>
    where
        To: 'static,
    {
        let from_type = from.ret_type;
        let to_type = TDesc::of::<To>();
        self.cast_to(RetType::of_val::<To>(), from)
            .map_err(|_| format!("Cannot cast {from_type} to {to_type}"))
    }

    // try cast expression so that it returns type to_type
    fn cast_to(&self, to_type: RetType, from: DynBoxedFn) -> Result<DynBoxedFn, DynBoxedFn> {
        // if types are the same just return from unchanged
        if from.ret_type == to_type {
            return Ok(from);
        }

        // lookup cast in registered casts
        let Some(cast_fn) = self.inner.casts.get(&(from.ret_type, to_type)) else {
            return Err(from);
        };

        // cast_fn takes Fn(Ctx)->From and return Fn(Ctx)->To
        cast_fn(&from).ok_or(from)
    }

    // attempt to make two expressions be of the same type by casting either of them
    fn cast_same_type(
        &self,
        a: DynBoxedFn,
        b: DynBoxedFn,
    ) -> CompilerResult<(DynBoxedFn, DynBoxedFn)> {
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
}

impl<Ctx> Default for Compiler<Ctx>
where
    Ctx: SupportedType,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Ctx> CompilerInner<Ctx>
where
    Ctx: SupportedType + 'static,
{
    // check if T is registered and call T::register if not
    pub fn register_type<T: SupportedType + 'static>(&mut self) {
        let ty = TDesc::of::<T>();
        if self.registered_types.contains(&ty) {
            return;
        }
        self.registered_types.insert(ty);
        T::register(self);
    }

    // register a cast, should be called from SupportedType::register
    pub fn register_cast<From, To>(
        &mut self,
        cast_fn: impl Fn(From) -> To + Copy + 'static,
    ) -> &mut Self
    where
        From: 'static,
        To: 'static,
    {
        self.casts.insert(
            (RetType::of_val::<From>(), RetType::of_val::<To>()),
            Box::new(move |from| {
                // this function takes function Fn(Ctx)->From and return function Fn(Ctx)->To

                let from_fn = from.get_fn_ret_val::<Ctx, From>().ok()?;

                Some(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                    cast_fn(from_fn.call(ctx))
                }))
            }),
        );
        self
    }

    // register unary operator, should be called from SupportedType::register
    pub fn register_un_op<T>(
        &mut self,
        un_op: UnOp,
        unop_fn: impl Fn(T) -> T + Copy + 'static,
    ) -> &mut Self
    where
        T: 'static,
    {
        self.un_ops.insert(
            (RetType::of_val::<T>(), un_op),
            Box::new(move |rhs| {
                // this function takes takes and returns functions Fn(Ctx)->T
                // returned function applies unary operation to result of the rhs function

                let rhs_expr = rhs.get_fn_ret_val::<Ctx, T>()?;

                Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                    unop_fn(rhs_expr.call(ctx))
                }))
            }),
        );
        self
    }

    // register binary operator, should be called from SupportedType::register
    pub fn register_bin_op<T>(
        &mut self,
        bin_op: BinOp,
        bin_op_fn: impl Fn(T, T) -> T + Copy + 'static,
    ) -> &mut Self
    where
        T: 'static,
    {
        self.bin_ops.insert(
            (RetType::of_val::<T>(), bin_op),
            Box::new(
                move |lhs: DynBoxedFn, rhs: DynBoxedFn| -> CompilerResult<DynBoxedFn> {
                    // this function takes rhs and lhs function of type Fn(Ctx)->T
                    // return function Fn(Ctx)->T which returns the result of the binary operation

                    let lhs_fn = lhs.get_fn_ret_val::<Ctx, T>()?;

                    let rhs_fn = rhs.get_fn_ret_val::<Ctx, T>()?;

                    Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                        bin_op_fn(lhs_fn.call(ctx), rhs_fn.call(ctx))
                    }))
                },
            ),
        );
        self
    }

    // register field access, should be called from SupportedType::register
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

        let obj_type = RetType::of_ref::<Obj>();
        self.fields.insert(
            (obj_type, field_name),
            Box::new(move |obj: DynBoxedFn| -> CompilerResult<DynBoxedFn> {
                // this function takes function of type Fn(Ctx)->Obj
                // and returns function of type Fn(Ctx)->Field

                let obj_fn = obj.get_fn_ret_ref::<Ctx, Obj>()?;

                Ok(Field::make_getter(move |ctx: &Ctx| {
                    getter(obj_fn.call(ctx))
                }))
            }),
        );
        self
    }

    pub fn register_function<A, R, F>(
        &mut self,
        prepare_args: impl Fn(&Compiler<Ctx>, Vec<DynBoxedFn>) -> CompilerResult<DynBoxedFn> + 'static,
        call: impl Fn(&F, A) -> R + Copy + 'static,
    ) where
        F: 'static,
        A: 'static,
        R: 'static,
    {
        self.functions.insert(
            RetType::of_ref::<F>(),
            Box::new(move |compiler, func, args| {
                let func = func.get_fn_ret_ref::<Ctx, F>()?;
                let args = prepare_args(compiler, args)?;
                let args = args.get_fn_ret_val()?;
                Ok(DynBoxedFn::make_ret_val(move |ctx: &Ctx| {
                    let func = func.call(ctx);
                    let args = args.call(ctx);
                    call(func, args)
                }))
            }),
        );
    }
}
