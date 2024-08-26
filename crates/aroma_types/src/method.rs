//! methods are parts of a vtable for a type

use crate::class::ClassInst;
use crate::generic::GenericDeclaration;
use crate::vis::{Vis, Visibility};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use crate::type_signature::TypeSignature;

pub type MethodId = u64;

#[derive(Clone, Debug)]
pub struct MethodDeclaration {
    id: Arc<AtomicU64>,
    vis: Vis,
    name: String,
    generic_declaration: Vec<GenericDeclaration>,
    return_type: TypeSignature,
    parameters: Vec<Parameter>,
    throws: Vec<ClassInst>,
}

impl MethodDeclaration {
    /// Create a new method
    pub fn new(
        vis: Vis,
        name: impl AsRef<str>,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        return_type: impl Into<TypeSignature>,
        parameters: impl IntoIterator<Item = Parameter>,
        throws: impl IntoIterator<Item = ClassInst>,
    ) -> Self {
        Self {
            id: Arc::new(AtomicU64::new(0)),
            vis,
            name: name.as_ref().to_string(),
            generic_declaration: generic_declaration.into_iter().collect(),
            return_type: return_type.into(),
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

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn generic_declaration(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }

    pub fn return_type(&self) -> &TypeSignature {
        &self.return_type
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }

    pub fn throws(&self) -> &[ClassInst] {
        &self.throws
    }
}

impl Visibility for MethodDeclaration {
    fn visibility(&self) -> Vis {
        self.vis
    }

    fn visibility_mut(&mut self) -> &mut Vis {
        &mut self.vis
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub name: String,
    pub signature: TypeSignature,
}
