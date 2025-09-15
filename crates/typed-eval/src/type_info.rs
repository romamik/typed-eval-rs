use std::{any::TypeId, hash::Hash};

#[derive(Clone, Copy)]
pub struct TypeInfo {
    pub type_id: TypeId,
    pub type_name: &'static str,
}

impl TypeInfo {
    pub fn new<T: 'static>(type_name: &'static str) -> Self {
        Self {
            type_id: TypeId::of::<T>(),
            type_name,
        }
    }
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_id = self.type_id;
        write!(f, "TypeId({self}, {type_id:?})")
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
