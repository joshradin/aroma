use crate::method::MethodDef;
use crate::references::{GetInfoTypeRef, NameType};
use aroma_tokens::spanned::{Span, Spanned};
use aroma_types::class::Class;
use std::collections::HashMap;

/// A top level item
#[derive(Debug)]
pub enum Item {
    /// A class item
    Class(ItemClass),
}

#[derive(Debug)]
pub struct ItemClass {
    span: Span,
    /// The class definition
    pub class: Class,
    pub method_definitions: HashMap<NameType, MethodDef>,
}

impl ItemClass {
    /// Creates an item class
    pub fn new<I: IntoIterator<Item = MethodDef>>(span: Span, class: Class, methods: I) -> Self {
        Self {
            span,
            class,
            method_definitions: methods
                .into_iter()
                .map(|cls| (cls.get_info_type_ref(), cls))
                .collect(),
        }
    }
}

impl Spanned for ItemClass {
    fn span(&self) -> Span {
        self.span.clone()
    }
}
