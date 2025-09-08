use crate::{BinOp, DynFn, ExprContext, UnOp};
use std::{any::TypeId, collections::HashMap, marker::PhantomData};

type CastKey = (TypeId, TypeId);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type UnOpKey = (UnOp, TypeId);
type CompileUnOpFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type BinOpKey = (BinOp, TypeId);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn, String>>;

type FieldAccessKey = (TypeId, &'static str);
type FieldAccessFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

pub struct CompilerRegistry<Ctx> {
    pub(crate) casts: HashMap<CastKey, CompileCastFunc>,
    pub(crate) unary_operations: HashMap<UnOpKey, CompileUnOpFunc>,
    pub(crate) binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    pub(crate) field_access: HashMap<FieldAccessKey, FieldAccessFunc>,
    pub(crate) ctx_type: PhantomData<Ctx>,
}

impl<Ctx: ExprContext> Default for CompilerRegistry<Ctx> {
    fn default() -> Self {
        Self {
            casts: HashMap::new(),
            unary_operations: HashMap::new(),
            binary_operations: HashMap::new(),
            field_access: HashMap::new(),
            ctx_type: PhantomData,
        }
    }
}

impl<Ctx: ExprContext> CompilerRegistry<Ctx> {
    pub fn register_cast<From: 'static, To: 'static>(
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

    pub fn register_un_op<T: 'static>(
        &mut self,
        op: UnOp,
        un_op_fn: fn(T) -> T,
    ) {
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

    pub fn register_bin_op<T: 'static>(
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
}
