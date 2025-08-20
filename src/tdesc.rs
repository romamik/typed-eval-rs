use std::{
    any::{TypeId, type_name},
    hash::Hash,
};

#[derive(Clone, Copy)]
pub struct TDesc {
    type_id: TypeId,
    type_name: &'static str,
}

impl TDesc {
    pub fn of<T>() -> Self
    where
        T: 'static,
    {
        Self {
            type_id: TypeId::of::<T>(),
            type_name: type_name::<T>(),
        }
    }
}

impl PartialEq for TDesc {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

impl Eq for TDesc {}

impl Hash for TDesc {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_id.hash(state);
    }
}

impl std::fmt::Debug for TDesc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

impl std::fmt::Display for TDesc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}
