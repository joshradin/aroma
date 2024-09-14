//! methods are parts of a vtable for a type

use crate::class::ClassInst;
use crate::generic::GenericDeclaration;
use crate::type_signature::TypeSignature;
use crate::vis::{Vis, Visibility};
use aroma_tokens::spanned::{NoSpanError, Span, TrySpanned};
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
    signature: FunctionSignature,
}

impl FunctionDeclaration {
    /// Create a new method
    pub fn new(
        vis: Vis,
        name: impl AsRef<str>,
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        receiver: impl Into<Option<TypeSignature>>,
        parameters: impl IntoIterator<Item = TypeSignature>,
        return_type: impl Into<Option<TypeSignature>>,
        throws: impl IntoIterator<Item = TypeSignature>,
    ) -> Self {
        Self {
            id: Arc::new(AtomicU64::new(0)),
            vis,
            name: name.as_ref().to_string(),
            signature: FunctionSignature::new(
                generic_declaration,
                receiver,
                parameters,
                return_type,
                throws,
            ),
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

    pub fn receiver(&self) -> Option<&TypeSignature> {
        self.signature.receiver()
    }

    pub fn receiver_mut(&mut self) -> &mut Option<TypeSignature> {
        self.signature.receiver_mut()
    }

    pub fn generic_declarations(&self) -> &[GenericDeclaration] {
        &self.signature.generic_declaration
    }
    pub fn generic_declarations_mut(&mut self) -> &mut Vec<GenericDeclaration> {
        &mut self.signature.generic_declaration
    }

    pub fn return_type(&self) -> &TypeSignature {
        self.signature.return_type()
    }

    pub fn return_type_mut(&mut self) -> &mut TypeSignature {
        self.signature.return_type_mut()
    }

    pub fn parameters(&self) -> &[TypeSignature] {
        &self.signature.parameters()
    }
    pub fn parameters_mut(&mut self) -> &mut Vec<TypeSignature> {
        &mut self.signature.parameters
    }

    pub fn throws(&self) -> &[TypeSignature] {
        self.signature.throws()
    }
    pub fn throws_mut(&mut self) -> &mut Vec<TypeSignature> {
        self.signature.throws_mut()
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
        if let Some(receiver) = self.receiver() {
            builder = format!("{builder}{receiver}.")
        }
        builder = format!("{builder}({})", self.parameters().iter().join(","));
        builder = format!("{builder} -> {}", self.return_type());

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
    pub ts: TypeSignature,
}

impl Debug for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.name, self.ts)
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, ClassInst::from(self.ts.clone()))
    }
}

/// A function signature
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    generic_declaration: Vec<GenericDeclaration>,
    receiver: Option<TypeSignature>,
    parameters: Vec<TypeSignature>,
    return_type: TypeSignature,
    throws: Vec<TypeSignature>,
}

impl FunctionSignature {
    /// Creates a new function signature
    pub fn new(
        generic_declaration: impl IntoIterator<Item = GenericDeclaration>,
        receiver: impl Into<Option<TypeSignature>>,
        parameters: impl IntoIterator<Item = TypeSignature>,
        return_type: impl Into<Option<TypeSignature>>,
        throws: impl IntoIterator<Item = TypeSignature>,
    ) -> Self {
        Self {
            generic_declaration: generic_declaration.into_iter().collect(),
            receiver: receiver.into(),
            parameters: parameters.into_iter().collect(),
            return_type: return_type.into().unwrap_or_default(),
            throws: throws.into_iter().collect(),
        }
    }

    /// Declare a function with a given name and visibility using this function signature
    pub fn declare(self, vis: Vis, name: impl AsRef<str>) -> FunctionDeclaration {
        let Self {
            generic_declaration, receiver, parameters, return_type, throws
        } = self;

        FunctionDeclaration::new(
            vis,
            name,
            generic_declaration,
            receiver,
            parameters,
            return_type,
            throws
        )
    }

    /// Gets the generic parameters for this function
    pub fn generic_declaration(&self) -> &[GenericDeclaration] {
        &self.generic_declaration
    }

    /// Gets the generic parameters for this function
    pub fn generic_declaration_mut(&mut self) -> &mut Vec<GenericDeclaration> {
        &mut self.generic_declaration
    }

    /// Gets the optional receiver type for this function
    pub fn receiver(&self) -> Option<&TypeSignature> {
        self.receiver.as_ref()
    }

    /// Gets the optional receiver type for this function
    pub fn receiver_mut(&mut self) -> &mut Option<TypeSignature> {
        &mut self.receiver
    }

    /// Gets the types of the parameters for this function
    pub fn parameters(&self) -> &[TypeSignature] {
        &self.parameters
    }

    /// Gets the types of the parameters for this function
    pub fn parameters_mut(&mut self) -> &mut Vec<TypeSignature> {
        &mut self.parameters
    }

    /// The return type for this function
    pub fn return_type(&self) -> &TypeSignature {
        &self.return_type
    }

    /// The return type for this function
    pub fn return_type_mut(&mut self) -> &mut TypeSignature {
        &mut self.return_type
    }

    /// The list of possible throwable types for this function
    pub fn throws(&self) -> &[TypeSignature] {
        &self.throws
    }
    /// The list of possible throwable types for this function
    pub fn throws_mut(&mut self) -> &mut Vec<TypeSignature> {
        &mut self.throws
    }
}

fn flip_result(opt: Option<Result<Span, NoSpanError>>) -> Result<Option<Span>, NoSpanError> {
    match opt {
        None => Ok(None),
        Some(result) => result.map(Some),
    }
}

impl TrySpanned for FunctionSignature {
    type Error = NoSpanError;

    fn try_span(&self) -> Result<Span, Self::Error> {
        let generics = self.generic_declaration()
            .into_iter()
            .map(|gd| gd.bound().try_span())
            .collect::<Result<Vec<_>, _>>()?;
        let parameters = self.parameters()
            .into_iter()
            .map(|gd| gd.try_span())
            .collect::<Result<Vec<_>, _>>()?;

        let receiver = flip_result(self.receiver().map(TrySpanned::try_span))?;
        let return_type = self.return_type().try_span()?;

        receiver
            .into_iter()
            .chain(generics)
            .chain(parameters)
            .chain([return_type])
            .reduce(|a, b| a.join(b))
            .ok_or(NoSpanError)
    }
}
