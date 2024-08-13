//! methods are parts of a vtable for a type

use crate::class::ClassInst;
use crate::generic::GenericDeclaration;
use crate::vis::{Vis, Visibility};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

pub type MethodId = u64;

#[derive(Clone,Debug)]
pub struct Method {
    id: Arc<AtomicU64>,
    vis: Vis,
    name: String,
    generic_declaration: Vec<GenericDeclaration>,
    return_type: ClassInst,
    parameters: Vec<Parameter>,
}

impl Method {
    pub fn new(
        id: MethodId,
        vis: Vis,
        name: String,
        generic_declaration: Vec<GenericDeclaration>,
        return_type: ClassInst,
        parameters: Vec<Parameter>,
    ) -> Self {
        Self {
            id: Arc::new(AtomicU64::new(id)),
            vis,
            name,
            generic_declaration,
            return_type,
            parameters,
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

    pub fn return_type(&self) -> &ClassInst {
        &self.return_type
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }
}

impl Visibility for Method {
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
    pub class: ClassInst,
}
