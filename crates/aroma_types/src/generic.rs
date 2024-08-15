use crate::class::ClassInst;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct GenericDeclaration {
    id: String,
    bound: ClassInst,
}

impl GenericDeclaration {
    pub fn new(id: impl AsRef<str>, bound: ClassInst) -> Self {
        Self {
            id: id.as_ref().to_string(),
            bound,
        }
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn bound(&self) -> &ClassInst {
        &self.bound
    }

    pub fn as_invariant(&self) -> GenericParameterBound {
        GenericParameterBound::Invariant(self.bound.clone())
    }
    pub fn as_covariant(&self) -> GenericParameterBound {
        GenericParameterBound::Covariant(self.bound.clone())
    }
    pub fn as_contravariant(&self) -> GenericParameterBound {
        GenericParameterBound::Contravariant(self.bound.clone())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GenericParameterBound {
    Invariant(ClassInst),
    Covariant(ClassInst),
    Contravariant(ClassInst),
}

impl GenericParameterBound {
    pub fn bound_class_instance(&self) -> &ClassInst {
        match self {
            GenericParameterBound::Invariant(cr) => cr,
            GenericParameterBound::Covariant(cr) => cr,
            GenericParameterBound::Contravariant(cr) => cr,
        }
    }
    pub(crate) fn bound_class_instance_mut(&mut self) -> &mut ClassInst {
        match self {
            GenericParameterBound::Invariant(cr) => cr,
            GenericParameterBound::Covariant(cr) => cr,
            GenericParameterBound::Contravariant(cr) => cr,
        }
    }

    pub fn is_invariant(&self) -> bool {
        matches!(self, Self::Invariant(_))
    }

    pub fn is_covariant(&self) -> bool {
        matches!(self, Self::Covariant(_))
    }

    pub fn is_contravariant(&self) -> bool {
        matches!(self, Self::Contravariant(_))
    }
}

impl From<GenericDeclaration> for GenericParameterBound {
    fn from(value: GenericDeclaration) -> Self {
        value.as_invariant()
    }
}

impl Display for GenericParameterBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericParameterBound::Invariant(i) => {
                write!(f, "{i}")
            }
            GenericParameterBound::Covariant(c) => {
                write!(f, "? extends {c}")
            }
            GenericParameterBound::Contravariant(c) => {
                write!(f, "? super {c}")
            }
        }
    }
}

/// Parameter bounds
pub type GenericParameterBounds = Vec<GenericParameterBound>;
