use crate::class::ClassInst;
use crate::vis::{Vis, Visibility};
use std::fmt::{Debug, Formatter};
use crate::type_signature::TypeSignature;

/// A field in a class
#[derive(Clone)]
pub struct Field {
    vis: Vis,
    name: String,
    kind: TypeSignature,
    is_final: bool,
}

impl Field {
    /// Creates a new name
    pub fn new(vis: Vis, name: impl AsRef<str>, kind: impl Into<TypeSignature>) -> Self {
        Self {
            vis,
            name: name.as_ref().to_string(),
            kind: kind.into(),
            is_final: false,
        }
    }

    /// Creates a new field
    pub(crate) fn with_type(field: Field, kind:  impl Into<TypeSignature>) -> Self {
        Self { kind: kind.into(), ..field }
    }

    /// Creates a new field that's final
    pub fn new_final(vis: Vis, name: impl AsRef<str>, kind:  impl Into<TypeSignature>) -> Self {
        Self {
            vis,
            name: name.as_ref().to_string(),
            kind: kind.into(),
            is_final: true,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_signature(&self) -> &TypeSignature {
        &self.kind
    }
    pub fn type_signature_mut(&mut self) -> &mut TypeSignature {
        &mut self.kind
    }

    pub fn is_final(&self) -> bool {
        self.is_final
    }
}

impl Debug for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({}: {})", self.vis, self.name, self.kind)
    }
}

impl Visibility for Field {
    fn visibility(&self) -> Vis {
        self.vis
    }

    fn visibility_mut(&mut self) -> &mut Vis {
        &mut self.vis
    }
}
