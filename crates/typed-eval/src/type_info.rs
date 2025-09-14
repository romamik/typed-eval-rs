use std::{any::TypeId, borrow::Cow, hash::Hash};

#[derive(Clone, Copy)]
pub struct TypeInfo {
    pub type_id: TypeId,
    pub type_name: fn() -> Cow<'static, str>,
}

impl TypeInfo {
    pub fn new<T: 'static>(type_name: fn() -> Cow<'static, str>) -> Self {
        Self {
            type_id: TypeId::of::<T>(),
            type_name,
        }
    }

    pub fn type_name(&self) -> Cow<'static, str> {
        (self.type_name)()
    }
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeId({}, {:?})", self.type_name(), self.type_id)
    }
}

impl PartialEq for TypeInfo {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

impl Eq for TypeInfo {}

impl Hash for TypeInfo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_id.hash(state);
    }
}
