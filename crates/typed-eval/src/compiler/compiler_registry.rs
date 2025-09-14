use crate::{BinOp, DynFn, Error, EvalType, Result, TypeInfo, UnOp};
use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    hash::Hash,
    marker::PhantomData,
};

type CastKey = (TypeInfo, TypeInfo);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn>>;

type UnOpKey = (UnOp, TypeInfo);
type CompileUnOpFunc = Box<dyn Fn(DynFn) -> Result<DynFn>>;

type BinOpKey = (BinOp, TypeInfo);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn>>;

type FieldAccessKey = (TypeInfo, &'static str);
type FieldAccessFunc = Box<dyn Fn(DynFn) -> Result<DynFn>>;

type MethodCallKey = (TypeInfo, &'static str);

pub(crate) struct MethodCallData {
    pub compile_fn: Box<dyn Fn(DynFn, Vec<DynFn>) -> Result<DynFn>>,
    pub arg_types: Vec<TypeInfo>,
}

pub struct RegistryAccess<'r, Ctx, T> {
    pub(super) registry: &'r mut CompilerRegistry,
    ty: PhantomData<(Ctx, T)>,
}

impl<'r, Ctx, T> RegistryAccess<'r, Ctx, T> {
    pub(crate) fn new(registry: &'r mut CompilerRegistry) -> Self {
        Self {
            registry,
            ty: PhantomData,
        }
    }
}

#[derive(Default)]
pub(crate) struct CompilerRegistry {
    registered_types: HashSet<TypeInfo>,
    pub(crate) casts: HashMap<CastKey, CompileCastFunc>,
    pub(crate) unary_operations: HashMap<UnOpKey, CompileUnOpFunc>,
    pub(crate) binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    pub(crate) field_access: HashMap<FieldAccessKey, FieldAccessFunc>,
    pub(crate) method_calls: HashMap<MethodCallKey, MethodCallData>,
}

impl CompilerRegistry {
    pub fn register_type<Ctx: EvalType, T: EvalType>(&mut self) -> Result<()> {
        let type_id = T::type_info();

        if self.registered_types.insert(type_id) {
            T::register_methods(RegistryAccess::<Ctx, T>::new(self))?;
            T::register(RegistryAccess::<Ctx, T>::new(self))?;
        }

        Ok(())
    }
}

impl<'r, Ctx: EvalType, T: EvalType> RegistryAccess<'r, Ctx, T> {
    pub fn register_type<T2: EvalType>(&mut self) -> Result<()> {
        self.registry.register_type::<Ctx, T2>()
    }

    // cast from T to To
    pub fn register_cast<To>(
        &mut self,
        cast_fn: for<'a> fn(&'a Ctx, T::RefType<'a>) -> To::RefType<'a>,
    ) -> Result<()>
    where
        To: EvalType,
    {
        let (from, to) = (T::type_info(), To::type_info());

        let compile_func = Box::new(move |from: DynFn| -> Result<DynFn> {
            let from = from.downcast::<Ctx, T>()?;
            Ok(To::make_dyn_fn(move |ctx| cast_fn(ctx, from(ctx))))
        });

        try_insert(&mut self.registry.casts, (from, to), compile_func, || {
            Error::DuplicateCast { from, to }
        })
    }

    pub fn register_un_op(
        &mut self,
        op: UnOp,
        un_op_fn: for<'a> fn(&'a Ctx, T::RefType<'a>) -> T::RefType<'a>,
    ) -> Result<()> {
        let ty = T::type_info();

        let compile_func = Box::new(move |rhs: DynFn| -> Result<DynFn> {
            let rhs = rhs.downcast::<Ctx, T>()?;
            Ok(T::make_dyn_fn(move |ctx| un_op_fn(ctx, rhs(ctx))))
        });

        try_insert(
            &mut self.registry.unary_operations,
            (op, ty),
            compile_func,
            || Error::DuplicateUnOp { op, ty },
        )
    }

    pub fn register_bin_op(
        &mut self,
        op: BinOp,
        bin_op_fn: for<'a> fn(
            &'a Ctx,
            T::RefType<'a>,
            T::RefType<'a>,
        ) -> T::RefType<'a>,
    ) -> Result<()> {
        let ty = T::type_info();

        let compile_func =
            Box::new(move |lhs: DynFn, rhs: DynFn| -> Result<DynFn> {
                let lhs = lhs.downcast::<Ctx, T>()?;
                let rhs = rhs.downcast::<Ctx, T>()?;
                Ok(T::make_dyn_fn(move |ctx| {
                    bin_op_fn(ctx, lhs(ctx), rhs(ctx))
                }))
            });

        try_insert(
            &mut self.registry.binary_operations,
            (op, ty),
            compile_func,
            || Error::DuplicateBinOp { op, ty },
        )
    }

    // access field on type T
    pub fn register_field_access<Field>(
        &mut self,
        field: &'static str,
        getter: for<'a> fn(&'a T) -> Field::RefType<'a>,
    ) -> Result<()>
    where
        T: for<'a> EvalType<RefType<'a> = &'a T>,
        Field: EvalType,
    {
        self.register_type::<Field>()?;

        let ty = T::type_info();

        let compile_func = Box::new(move |obj: DynFn| -> Result<DynFn> {
            let obj = obj.downcast::<Ctx, T>()?;
            Ok(Field::make_dyn_fn(move |ctx| getter(obj(ctx))))
        });

        try_insert(
            &mut self.registry.field_access,
            (ty, field),
            compile_func,
            || Error::DuplicateField { ty, field },
        )
    }
}

pub(super) fn try_insert<K, V>(
    map: &mut HashMap<K, V>,
    key: K,
    value: V,
    make_err: impl FnOnce() -> Error,
) -> Result<()>
where
    K: Hash + Eq,
{
    match map.entry(key) {
        Entry::Occupied(_) => return Err(make_err()),
        Entry::Vacant(vacant) => {
            vacant.insert(value);
        }
    }
    Ok(())
}
