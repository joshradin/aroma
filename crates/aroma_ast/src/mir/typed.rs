//! typing information traits

/// Gets the type of the node within the tree
pub trait Typed<T, E = TypeError> {
    /// Gets the type
    fn get_type(&self) -> TypeState<&T, &E>;

}

/// Gets a type reference that's mutable
pub trait TypedMut<T, E = TypeError> : Typed<T, E> {
    /// Gets the type
    fn get_type_mut(&mut self) -> &mut TypeState<T, E>;

    /// Set the type for this object
    fn set_type(&mut self, ty: T) {
        *self.get_type_mut() = TypeState::Available(ty);
    }

    /// Sets an error for this object
    fn set_err(&mut self, e: E) {
        *self.get_type_mut() = TypeState::Err(e);
    }
}

/// The state of a type node
#[derive(Debug, Default, Clone)]
pub enum TypeState<T, E = TypeError> {
    #[default]
    Unavailable,
    Available(T),
    Err(E),
}


impl<T, E> TypeState<T, E> {
    /// Gets the internal representation as references
    pub fn as_ref(&self) -> TypeState<&T, &E> {
        match self {
            TypeState::Unavailable => TypeState::Unavailable,
            TypeState::Available(t) => TypeState::Available(t),
            TypeState::Err(e) => TypeState::Err(e),
        }
    }

    /// Gets the type, if available
    pub fn get_available(&self) -> Option<&T> {
        if let TypeState::Available(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

impl<T: Clone, E: Clone> TypeState<&T, &E> {
    /// Gets the internal representation as cloned
    pub fn cloned(&self) -> TypeState<T, E> {
        match self {
            TypeState::Unavailable => TypeState::Unavailable,
            TypeState::Available(t) => TypeState::Available((**t).clone()),
            TypeState::Err(e) => TypeState::Err((**e).clone()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Could not resolve identifier {0:?}")]
    IdentifierNotResolved(String)
}
