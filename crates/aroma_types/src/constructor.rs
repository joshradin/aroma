//! Constructors are used for creating objects

use crate::class::ClassInst;
use crate::generic::GenericDeclaration;
use crate::method::{MethodId, Parameter};
use crate::vis::Vis;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// Constructors are specialty methods that have no return type
#[derive(Debug, Clone)]
pub struct Constructor {
    id: Arc<AtomicU64>,
    vis: Vis,
    generic_declaration: Vec<GenericDeclaration>,
    parameters: Vec<Parameter>,
    throws: Vec<ClassInst>,
}

impl Constructor {
    /// Create a new constructor
    pub fn new(
        id: MethodId,
        vis: Vis,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        parameters: impl IntoIterator<Item = Parameter>,
        throws: impl IntoIterator<Item = ClassInst>,
    ) -> Self {
        Self {
            id: Arc::new(AtomicU64::new(id)),
            vis,
            generic_declaration: generic_declaration.into_iter().collect(),
            parameters: parameters.into_iter().collect(),
            throws: throws.into_iter().collect(),
        }
    }

    pub fn id(&self) -> MethodId {
        self.id.load(Ordering::SeqCst)
    }

    /// Sets the id of the method
    pub fn set_id(&mut self, id: MethodId) {
        self.id.store(id, Ordering::SeqCst);
    }

    pub fn generic_declaration(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn throws(&self) -> &[ClassInst] {
        &self.throws
    }
}
