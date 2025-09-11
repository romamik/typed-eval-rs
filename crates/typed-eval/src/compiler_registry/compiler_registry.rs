use crate::{BinOp, DynFn, SupportedType, UnOp};
use std::{
    any::TypeId,
    collections::{HashMap, HashSet, hash_map::Entry},
    hash::Hash,
    marker::PhantomData,
};

type CastKey = (TypeId, TypeId);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type UnOpKey = (UnOp, TypeId);
type CompileUnOpFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type BinOpKey = (BinOp, TypeId);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn, String>>;

type FieldAccessKey = (TypeId, &'static str);
type FieldAccessFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type MethodCallKey = (TypeId, &'static str);
pub(crate) struct MethodCallData {
    pub compile_fn: Box<dyn Fn(DynFn, Vec<DynFn>) -> Result<DynFn, String>>,
    pub arg_types: Vec<TypeId>,
}

// Registry access is passed to SupportedType::register()
// instead of just passing CompilerRegistry
//
// * it prevents calling SupportedType::register() directly, without using register_type()
// * it already has T type parameter that is needed for most of functions
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
    registered_types: HashSet<TypeId>,
    pub(crate) casts: HashMap<CastKey, CompileCastFunc>,
    pub(crate) unary_operations: HashMap<UnOpKey, CompileUnOpFunc>,
    pub(crate) binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    pub(crate) field_access: HashMap<FieldAccessKey, FieldAccessFunc>,
    pub(crate) method_calls: HashMap<MethodCallKey, MethodCallData>,
}

impl CompilerRegistry {
    pub fn register_type<Ctx: SupportedType, T: SupportedType>(
        &mut self,
    ) -> Result<(), String> {
        let type_id = TypeId::of::<T>();
        if self.registered_types.insert(type_id) {
            T::register_methods(RegistryAccess::<Ctx, T>::new(self))?;
            T::register(RegistryAccess::<Ctx, T>::new(self))?;
        }
        Ok(())
    }
}

impl<'r, Ctx: SupportedType, T: SupportedType> RegistryAccess<'r, Ctx, T> {
    pub fn register_type<T2: SupportedType>(&mut self) -> Result<(), String> {
        self.registry.register_type::<Ctx, T2>()
    }

    // cast from T to To
    pub fn register_cast<To>(
        &mut self,
        cast_fn: for<'a> fn(&'a Ctx, T::RefType<'a>) -> To::RefType<'a>,
    ) -> Result<(), String>
    where
        To: SupportedType,
    {
        let key = (TypeId::of::<T>(), TypeId::of::<To>());
        let compile_func =
            Box::new(move |from: DynFn| -> Result<DynFn, String> {
                let from = from
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: from type mistmatch")?;
                Ok(To::make_dyn_fn(move |ctx| cast_fn(ctx, from(ctx))))
            });

        try_insert(&mut self.registry.casts, key, compile_func)
    }

    pub fn register_un_op(
        &mut self,
        op: UnOp,
        un_op_fn: for<'a> fn(&'a Ctx, T::RefType<'a>) -> T::RefType<'a>,
    ) -> Result<(), String> {
        let key = (op, TypeId::of::<T>());
        let compile_func =
            Box::new(move |rhs: DynFn| -> Result<DynFn, String> {
                let rhs = rhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: rhs type mistmatch")?;
                Ok(T::make_dyn_fn(move |ctx| un_op_fn(ctx, rhs(ctx))))
            });

        try_insert(&mut self.registry.unary_operations, key, compile_func)
    }

    pub fn register_bin_op(
        &mut self,
        op: BinOp,
        bin_op_fn: for<'a> fn(
            &'a Ctx,
            T::RefType<'a>,
            T::RefType<'a>,
        ) -> T::RefType<'a>,
    ) -> Result<(), String> {
        let key = (op, TypeId::of::<T>());
        let compile_func =
            Box::new(move |lhs: DynFn, rhs: DynFn| -> Result<DynFn, String> {
                let lhs = lhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: lhs type mistmatch")?;
                let rhs = rhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: rhs type mistmatch")?;
                Ok(T::make_dyn_fn(move |ctx| {
                    bin_op_fn(ctx, lhs(ctx), rhs(ctx))
                }))
            });

        try_insert(&mut self.registry.binary_operations, key, compile_func)
    }

    // access field on type T
    pub fn register_field_access<Field>(
        &mut self,
        field_name: &'static str,
        field_getter: for<'a> fn(&'a T) -> Field::RefType<'a>,
    ) -> Result<(), String>
    where
        T: for<'a> SupportedType<RefType<'a> = &'a T>,
        Field: SupportedType,
    {
        let key = (TypeId::of::<T>(), field_name);
        let compile_func =
            Box::new(move |obj: DynFn| -> Result<DynFn, String> {
                let obj = obj
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: obj type mistmatch")?;
                Ok(Field::make_dyn_fn(move |ctx| field_getter(obj(ctx))))
            });

        try_insert(&mut self.registry.field_access, key, compile_func)
    }
}

pub(super) fn try_insert<K, V>(
    map: &mut HashMap<K, V>,
    key: K,
    value: V,
) -> Result<(), String>
where
    K: Hash + Eq,
{
    match map.entry(key) {
        Entry::Occupied(_) => Err("Already exists")?,
        Entry::Vacant(vacant) => {
            vacant.insert(value);
        }
    }
    Ok(())
}
