//! type resolution

use aroma_ast::mir::typed::TypeError;
use crate::parser::semantic_transform::Transformer;
use crate::type_resolution::Bindings;

/// Type resolver
#[derive(Debug)]
pub struct TypeResolver<'p> {
    context: Bindings<'p>
}

impl<'p> Transformer<(), (), TypeError> for TypeResolver<'p> {
    fn transform(&mut self, i: ()) -> Result<((), ()), TypeError> {
        todo!()
    }
}

impl<'p> TypeResolver<'p> {
    /// Creates a new type resolver
    pub fn new() -> Self {
        Self { context: Bindings::new() }
    }
}
