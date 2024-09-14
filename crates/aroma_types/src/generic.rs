use crate::hierarchy::intrinsics::OBJECT_CLASS;
use crate::type_signature::TypeSignature;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct GenericDeclaration {
    id: String,
    bound: TypeSignature,
}

impl Display for GenericDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.bound == OBJECT_CLASS.generic_inst().into() {
            write!(f, "{}", self.id)
        } else {
            write!(f, "{} {}", self.id, self.bound)
        }
    }
}

impl GenericDeclaration {
    pub fn new(id: impl AsRef<str>, bound: TypeSignature) -> Self {
        Self {
            id: id.as_ref().to_string(),
            bound,
        }
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn bound(&self) -> &TypeSignature {
        &self.bound
    }
    pub fn bound_mut(&mut self) -> &mut TypeSignature {
        &mut self.bound
    }

}



/// Parameter bounds
pub type GenericParameterBounds = Vec<TypeSignature>;
pub type GenericParameterBoundsSlice = [TypeSignature];
