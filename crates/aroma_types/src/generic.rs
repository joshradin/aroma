use crate::class::ClassInst;
use crate::type_signature::TypeSignature;
use std::fmt::{Display, Formatter};
use crate::hierarchy::intrinsics::{BASE_CLASS_NAME, OBJECT_CLASS};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct GenericDeclaration {
    id: String,
    bound: ClassInst,
}

impl Display for GenericDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.bound.class_ref() == &OBJECT_CLASS.get_ref() {
            write!(f, "{}", self.id)
        } else {
            write!(f, "{} {}", self.id, self.bound)
        }
    }
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

impl From<TypeSignature> for GenericParameterBound {
    fn from(value: TypeSignature) -> Self {
        match value {
            TypeSignature::Invariant(c, p) => GenericParameterBound::Invariant(
                ClassInst::with_generics(c, p.into_iter().map(|tp| tp.into())),
            ),
            TypeSignature::Covariant(c, p) => GenericParameterBound::Covariant(
                ClassInst::with_generics(c, p.into_iter().map(|tp| tp.into())),
            ),
            TypeSignature::Contravariant(c, p) => GenericParameterBound::Contravariant(
                ClassInst::with_generics(c, p.into_iter().map(|tp| tp.into())),
            ),
            base => GenericParameterBound::Covariant(ClassInst::from(base)),
        }
    }
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
                write!(f, "out {c}")
            }
            GenericParameterBound::Contravariant(c) => {
                write!(f, "in {c}")
            }
        }
    }
}

/// Parameter bounds
pub type GenericParameterBounds = Vec<GenericParameterBound>;
