use crate::{BinOp, DynFn, SupportedType, UnOp};
use std::{
    any::TypeId,
    collections::{HashMap, HashSet, hash_map::Entry},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

type CastKey = (TypeId, TypeId);
type CompileCastFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type UnOpKey = (UnOp, TypeId);
type CompileUnOpFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

type BinOpKey = (BinOp, TypeId);
type CompileBinOpFunc = Box<dyn Fn(DynFn, DynFn) -> Result<DynFn, String>>;

type FieldAccessKey = (TypeId, &'static str);
type FieldAccessFunc = Box<dyn Fn(DynFn) -> Result<DynFn, String>>;

// Registry access is needed to prevent calling
// calling SupportedType::register()
// without calling registry.register_type()
pub struct RegistryAccess<'a, Ctx, T> {
    registry: &'a mut CompilerRegistry<Ctx>,
    ty: PhantomData<T>,
}

impl<'a, Ctx, T> Deref for RegistryAccess<'a, Ctx, T> {
    type Target = CompilerRegistry<Ctx>;
    fn deref(&self) -> &Self::Target {
        self.registry
    }
}

impl<'a, Ctx, T> DerefMut for RegistryAccess<'a, Ctx, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.registry
    }
}

pub struct CompilerRegistry<Ctx> {
    registered_types: HashSet<TypeId>,
    pub(crate) casts: HashMap<CastKey, CompileCastFunc>,
    pub(crate) unary_operations: HashMap<UnOpKey, CompileUnOpFunc>,
    pub(crate) binary_operations: HashMap<BinOpKey, CompileBinOpFunc>,
    pub(crate) field_access: HashMap<FieldAccessKey, FieldAccessFunc>,
    pub(crate) ctx_type: PhantomData<Ctx>,
}

impl<Ctx: SupportedType> Default for CompilerRegistry<Ctx> {
    fn default() -> Self {
        Self {
            registered_types: HashSet::new(),
            casts: HashMap::new(),
            unary_operations: HashMap::new(),
            binary_operations: HashMap::new(),
            field_access: HashMap::new(),
            ctx_type: PhantomData,
        }
    }
}

impl<Ctx: SupportedType> CompilerRegistry<Ctx> {
    pub fn register_type<T: SupportedType>(&mut self) -> Result<(), String> {
        let type_id = TypeId::of::<T>();
        if self.registered_types.contains(&type_id) {
            return Ok(());
        }
        self.registered_types.insert(type_id);
        T::register(RegistryAccess {
            registry: self,
            ty: PhantomData,
        })?;
        Ok(())
    }

    pub fn register_cast<From, To>(
        &mut self,
        cast_fn: for<'a> fn(&'a Ctx, From::RefType<'a>) -> To::RefType<'a>,
    ) -> Result<(), String>
    where
        From: SupportedType,
        To: SupportedType,
    {
        let key = (TypeId::of::<From>(), TypeId::of::<To>());
        let compile_func =
            Box::new(move |from: DynFn| -> Result<DynFn, String> {
                let from = from
                    .downcast::<Ctx, From>()
                    .ok_or("Compiler error: from type mistmatch")?;
                Ok(To::make_dyn_fn(move |ctx| cast_fn(ctx, from(ctx))))
            });

        match self.casts.entry(key) {
            Entry::Occupied(_) => Err("Cast already exists")?,
            Entry::Vacant(vacant) => {
                vacant.insert(compile_func);
            }
        }
        Ok(())
    }

    pub fn register_un_op<T>(
        &mut self,
        op: UnOp,
        un_op_fn: for<'a> fn(&'a Ctx, T::RefType<'a>) -> T::RefType<'a>,
    ) -> Result<(), String>
    where
        T: SupportedType,
    {
        let key = (op, TypeId::of::<T>());
        let compile_func =
            Box::new(move |rhs: DynFn| -> Result<DynFn, String> {
                let rhs = rhs
                    .downcast::<Ctx, T>()
                    .ok_or("Compiler error: rhs type mistmatch")?;
                Ok(T::make_dyn_fn(move |ctx| un_op_fn(ctx, rhs(ctx))))
            });

        match self.unary_operations.entry(key) {
            Entry::Occupied(_) => Err("Unary operation already exists")?,
            Entry::Vacant(vacant) => {
                vacant.insert(compile_func);
            }
        }
        Ok(())
    }

    pub fn register_bin_op<T>(
        &mut self,
        op: BinOp,
        bin_op_fn: for<'a> fn(
            &'a Ctx,
            T::RefType<'a>,
            T::RefType<'a>,
        ) -> T::RefType<'a>,
    ) -> Result<(), String>
    where
        T: SupportedType,
    {
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

        match self.binary_operations.entry(key) {
            Entry::Occupied(_) => Err("Binary operation already exists")?,
            Entry::Vacant(vacant) => {
                vacant.insert(compile_func);
            }
        }
        Ok(())
    }

    pub fn register_field_access<Obj, Field>(
        &mut self,
        field_name: &'static str,
        field_getter: for<'a> fn(
            &'a Ctx,
            Obj::RefType<'a>,
        ) -> Field::RefType<'a>,
    ) -> Result<(), String>
    where
        Obj: SupportedType,
        Field: SupportedType,
    {
        let key = (TypeId::of::<Obj>(), field_name);
        let compile_func =
            Box::new(move |obj: DynFn| -> Result<DynFn, String> {
                let obj = obj
                    .downcast::<Ctx, Obj>()
                    .ok_or("Compiler error: obj type mistmatch")?;
                Ok(Field::make_dyn_fn(move |ctx| field_getter(ctx, obj(ctx))))
            });

        match self.field_access.entry(key) {
            Entry::Occupied(_) => Err("Field access already exists")?,
            Entry::Vacant(vacant) => {
                vacant.insert(compile_func);
            }
        }
        Ok(())
    }
}
