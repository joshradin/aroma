//! translation units are the highest level

use crate::id::Id;
use crate::mir::items::Item;
use crate::spanned::Span;

/// A translation unit
#[derive(Debug)]
pub struct TranslationUnit {
    span: Span,
    pub namespace: Option<Id>,
    pub imports: Vec<Id>,
    pub items: Vec<Item>,
}

impl TranslationUnit {
    /// Creates a new translation unit
    pub fn new(span: Span, namespace: Option<Id>, imports: Vec<Id>, items: Vec<Item>) -> Self {
        Self {
            span,
            namespace,
            imports,
            items,
        }
    }
}
