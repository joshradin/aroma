//! methods are parts of a vtable for a type

use crate::class::ClassInst;
use crate::generic::GenericDeclaration;
use crate::type_signature::TypeSignature;
use crate::vis::{Vis, Visibility};
use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

pub type FunctionId = u64;

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    id: Arc<AtomicU64>,
    vis: Vis,
    name: String,
    generic_declaration: Vec<GenericDeclaration>,
    receiver: Option<ClassInst>,
    parameters: Vec<Parameter>,
    return_type: Option<ClassInst>,
    throws: Vec<ClassInst>,
}

impl FunctionDeclaration {
    /// Create a new method
    pub fn new(
        vis: Vis,
        name: impl AsRef<str>,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        receiver: impl Into<Option<ClassInst>>,
        parameters: impl IntoIterator<Item = Parameter>,
        return_type: impl Into<Option<ClassInst>>,
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
            receiver: receiver.into(),
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.load(Ordering::SeqCst)
    }

    /// Sets the id of the method
    pub fn set_id(&mut self, id: FunctionId) {
        self.id.store(id, Ordering::SeqCst);
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn generic_declarations(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }
    pub fn generic_declarations_mut(&mut self) -> &mut Vec<GenericDeclaration> {
        &mut self.generic_declaration
    }

    pub fn return_type(&self) -> Option<&ClassInst> {
        self.return_type.as_ref()
    }

    pub fn return_type_mut(&mut self) -> Option<&mut ClassInst> {
        self.return_type.as_mut()
    }

    pub fn parameters(&self) -> &[Parameter] {
        &self.parameters
    }
    pub fn parameters_mut(&mut self) -> &mut Vec<Parameter> {
        &mut self.parameters
    }

    pub fn throws(&self) -> &[ClassInst] {
        &self.throws
    }
    pub fn throws_mut(&mut self) -> &mut Vec<ClassInst> {
        &mut self.throws
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut builder = String::new();
        builder = format!("{builder}{}", self.name);
        if !self.generic_declarations().is_empty() {
            builder = format!(
                "{builder}[{}]",
                self.generic_declarations().iter().join(",")
            );
        }
        builder = format!("{builder}({})", self.parameters().iter().join(","));

        match self.return_type() {
            None => {}
            Some(ret) => builder = format!("{builder} -> {ret}"),
        }

        write!(f, "{}", builder)
    }
}

impl Visibility for FunctionDeclaration {
    fn visibility(&self) -> Vis {
        self.vis
    }

    fn visibility_mut(&mut self) -> &mut Vis {
        &mut self.vis
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub name: String,
    pub class: ClassInst,
}

impl Debug for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.name, self.class)
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, ClassInst::from(self.class.clone()))
    }
}
