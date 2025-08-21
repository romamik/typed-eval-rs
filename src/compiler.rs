use crate::{
    boxed_fn::{BoxedFnRetVal, DynBoxedFn},
    expr::{BinOp, Expr, UnOp},
    supported_type::SupportedType,
    tdesc::TDesc,
};
use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

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
            (TDesc::of::<From>(), TDesc::of::<To>()),
            Box::new(move |from| {
                // this function takes function Fn(Arg)->From and return function Fn(Arg)->To

                let from_fn = from.get_fn_ret_val::<Arg, From>().ok()?;

                Some(DynBoxedFn::make_ret_val(move |arg: &Arg| {
                    cast_fn(from_fn.call(arg))
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
            (TDesc::of::<T>(), un_op),
            Box::new(move |rhs| {
                // this function takes takes and returns functions Fn(Arg)->T
                // returned function applies unary operation to result of the rhs function

                let rhs_expr = rhs.get_fn_ret_val::<Arg, T>()?;

                Ok(DynBoxedFn::make_ret_val(move |arg: &Arg| {
                    unop_fn(rhs_expr.call(arg))
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
            (TDesc::of::<T>(), bin_op),
            Box::new(
                move |lhs: DynBoxedFn, rhs: DynBoxedFn| -> CompilerResult<DynBoxedFn> {
                    // this function takes rhs and lhs function of type Fn(Arg)->T
                    // return function Fn(Arg)->T which returns the result of the binary operation

                    let lhs_fn = lhs.get_fn_ret_val::<Arg, T>()?;

                    let rhs_fn = rhs.get_fn_ret_val::<Arg, T>()?;

                    Ok(DynBoxedFn::make_ret_val(move |arg: &Arg| {
                        bin_op_fn(lhs_fn.call(arg), rhs_fn.call(arg))
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

        let obj_type = TDesc::of::<Obj>();
        self.fields.insert(
            (obj_type, field_name),
            Box::new(move |obj: DynBoxedFn| -> CompilerResult<DynBoxedFn> {
                // this function takes function of type Fn(Arg)->Obj
                // and returns function of type Fn(Arg)->Field

                let obj_fn = obj.get_fn_ret_ref::<Arg, Obj>()?;

                Ok(Field::make_getter(move |arg: &Arg| {
                    getter(obj_fn.call(arg))
                }))
            }),
        );
        self
    }

    // try cast expression so that it returns type To
    fn cast<To>(&self, from: DynBoxedFn) -> CompilerResult<DynBoxedFn>
    where
        To: 'static,
    {
        let from_type = from.ret_type;
        let to_type = TDesc::of::<To>();
        self.cast_to(TDesc::of::<To>(), from)
            .map_err(|_| format!("Cannot cast {from_type} to {to_type}"))
    }

    // try cast expression so that it returns type to_type
    fn cast_to(&self, to_type: TDesc, from: DynBoxedFn) -> Result<DynBoxedFn, DynBoxedFn> {
        // if types are the same just return from unchanged
        if from.ret_type == to_type {
            return Ok(from);
        }

        // lookup cast in registered casts
        let Some(cast_fn) = self.casts.get(&(from.ret_type, to_type)) else {
            return Err(from);
        };

        // cast_fn takes Fn(Arg)->From and return Fn(Arg)->To
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

    fn compile_field_access(
        &self,
        obj: DynBoxedFn,
        field_name: &str,
    ) -> CompilerResult<DynBoxedFn> {
        let obj_type = obj.ret_type;
        if !obj.ret_ref {
            return Err("Invalid field access: need reference type".to_string());
        }
        let Some(field_fn) = self.fields.get(&(obj_type, field_name)) else {
            return Err(format!("No field {field_name} on type {obj_type}"));
        };

        field_fn(obj)
    }

    // compile AST to DynBoxedFn, the return type is determined from expression
    pub fn compile_typed(&self, expr: &Expr) -> CompilerResult<DynBoxedFn> {
        Ok(match expr {
            &Expr::Int(val) => DynBoxedFn::make_ret_val(move |_: &Arg| val),

            &Expr::Float(val) => DynBoxedFn::make_ret_val(move |_: &Arg| val),

            Expr::Var(name) => {
                self.compile_field_access(DynBoxedFn::make_ret_ref(|arg: &Arg| arg), name)?
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
                self.compile_field_access(obj, field_name)?
            }
        })
    }

    // compile AST to a function with a known return type
    // can result in cast error if expression resolves to incompatible type
    pub fn compile<Ret>(&self, expr: &Expr) -> CompilerResult<BoxedFnRetVal<Arg, Ret>>
    where
        Ret: 'static,
    {
        let compiled_expr = self.compile_typed(expr)?;
        let casted_fn = self.cast::<Ret>(compiled_expr)?.get_fn_ret_val()?;
        Ok(casted_fn)
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

pub type CompilerResult<T> = Result<T, String>;

type CastFn = Box<dyn Fn(&DynBoxedFn) -> Option<DynBoxedFn>>;
type UnopFn = Box<dyn Fn(DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
type BinopFn = Box<dyn Fn(DynBoxedFn, DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
type FieldFn = Box<dyn Fn(DynBoxedFn) -> CompilerResult<DynBoxedFn>>;
