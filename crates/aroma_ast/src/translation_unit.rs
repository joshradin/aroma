//! translation units are the highest level

use crate::items::Item;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::{Span, Spanned};

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

impl Spanned for TranslationUnit {
    fn span(&self) -> Span {
        self.span.clone()
    }
}