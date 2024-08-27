//! typing information traits

use aroma_types::type_signature::TypeSignature;

/// Represents some object with type information
pub trait TypeInfo: Clone {
    /// Gets the type signature
    fn signature(&self) -> TypeSignature;
}

impl TypeInfo for TypeSignature {
    fn signature(&self) -> TypeSignature {
        self.clone()
    }
}

/// Gets the type of the node within the tree
pub trait Typed<T: TypeInfo, E: Clone = TypeError> {
    /// Gets the type
    fn get_type(&self) -> TypeState<T, E>;
}



/// Gets a type reference that's mutable
pub trait TypedMut<T: TypeInfo, E: Clone = TypeError>: Typed<T, E> {
    /// Gets the type
    fn get_type_mut(&mut self) -> &mut TypeState<T, E>;

    /// Set the type for this object
    fn set_type(&mut self, ty: T) {
        *self.get_type_mut() = TypeState::Available(ty);
    }

    /// Sets the type for this object based on another
    fn clone_type_from<O: Typed<T, E2>, E2>(&mut self, other: O)
    where
        E2: Into<E> + Clone,
    {
        match other.get_type() {
            TypeState::Unavailable => {}
            TypeState::Available(t) => {
                self.set_type(t.clone());
            }
            TypeState::Err(e) => self.set_err(e.clone().into()),
        }
    }

    /// Sets an error for this object
    fn set_err(&mut self, e: E) {
        *self.get_type_mut() = TypeState::Err(e);
    }
}

/// The state of a type node
#[derive(Debug, Clone)]
pub enum TypeState<T, E = TypeError>
where
    T: Clone,
    E: Clone
{
    Unavailable,
    Available(T),
    Err(E),
}

impl<T, E> Default for TypeState<T, E>
where
    T: Clone,
    E: Clone
{
    fn default() -> Self {
        Self::Unavailable
    }
}

impl<T: Clone, E: Clone> TypeState<T, E> {
    /// Gets the internal representation as references
    pub fn as_ref(&self) -> TypeState<&T, &E> {
        match self {
            TypeState::Unavailable => TypeState::Unavailable,
            TypeState::Available(t) => TypeState::Available(t),
            TypeState::Err(e) => TypeState::Err(e),
        }
    }

    /// Gets the type, if available
    pub fn as_available(&self) -> Option<&T> {
        if let TypeState::Available(t) = self {
            Some(t)
        } else {
            None
        }
    }

    /// Gets the available type if present
    pub fn available(self) -> Option<T> {
        if let TypeState::Available(t) = self {
            Some(t)
        } else {
            None
        }
    }

    /// Maps the type, if available
    pub fn map<U, F>(self, mapper: F) -> TypeState<U, E>
    where
        F: FnOnce(T) -> U,
        U: Clone,
    {
        match self {
            TypeState::Unavailable => TypeState::Unavailable,
            TypeState::Available(a) => TypeState::Available(mapper(a)),
            TypeState::Err(e) => TypeState::Err(e),
        }
    }

    /// Maps the type info to it's signature
    #[inline]
    pub fn to_signature(self) -> TypeState<TypeSignature, E>
        where T : TypeInfo
    {
        self.map(|t| t.signature())
    }
}

impl<E : Clone> TypeState<TypeSignature, E> {

    /// Creates a never type state
    #[inline]
    pub fn never() -> Self {
        Self::Available(TypeSignature::Never)
    }
}

impl <T : TypeInfo> From<Option<T>> for TypeState<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            None => { TypeState::Unavailable }
            Some(v ) => { TypeState::Available(v)}
        }
    }
}

impl <T : TypeInfo> From<T> for TypeState<T> {
    fn from(value: T) -> Self {
        TypeState::Available(value)
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum TypeError {
    #[error("Could not resolve identifier {0:?}")]
    IdentifierNotResolved(String),
}
