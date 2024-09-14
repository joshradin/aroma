//! Constructors are used for creating objects

use crate::class::ClassInst;
use crate::functions::{FunctionId, FunctionSignature, Parameter};
use crate::generic::GenericDeclaration;
use crate::vis::Vis;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use crate::type_signature::TypeSignature;

/// Constructors are specialty methods that have no return type
#[derive(Debug, Clone)]
pub struct Constructor {
    id: Arc<AtomicU64>,
    vis: Vis,
    generic_declaration: Vec<GenericDeclaration>,
    parameters: Vec<Parameter>,
    throws: Vec<TypeSignature>,
    creates: TypeSignature,
}

impl Constructor {
    /// Create a new constructor
    pub fn new(
        id: FunctionId,
        vis: Vis,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        parameters: impl IntoIterator<Item = Parameter>,
        throws: impl IntoIterator<Item = TypeSignature>,
        creates: TypeSignature,
    ) -> Self {
        Self {
            id: Arc::new(AtomicU64::new(id)),
            vis,
            generic_declaration: generic_declaration.into_iter().collect(),
            parameters: parameters.into_iter().collect(),
            throws: throws.into_iter().collect(),
            creates,
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.load(Ordering::SeqCst)
    }

    /// Sets the id of the method
    pub fn set_id(&mut self, id: FunctionId) {
        self.id.store(id, Ordering::SeqCst);
    }

    pub fn vis(&self) -> Vis {
        self.vis
    }

    pub fn generic_declaration(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn throws(&self) -> &[TypeSignature] {
        &self.throws
    }

    pub fn creates(&self) -> &TypeSignature {
        &self.creates
    }

    /// Gets this constructor as a function signature
    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature::new(
            Vec::from(self.generic_declaration()),
            self.creates().clone(),
            self.parameters().iter().map(|p| p.ts.clone()),
            TypeSignature::Void,
            Vec::from(self.throws())
        )
    }


}
