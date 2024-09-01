use crate::method::MethodDef;
use crate::references::{GetInfoTypeRef, NameType};
use aroma_tokens::spanned::{Span, Spanned};
use aroma_types::class::Class;
use std::collections::HashMap;
use aroma_types::delegate::Delegate;

/// A top level item
#[derive(Debug)]
pub enum Item {
    /// A class item
    Class(ClassItem),
    Delegate(DelegateItem)
}

#[derive(Debug)]
pub struct ClassItem {
    span: Span,
    /// The class definition
    pub class: Class,
    pub method_definitions: HashMap<NameType, MethodDef>,
}

impl ClassItem {
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

impl Spanned for ClassItem {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct DelegateItem {
    span: Span,
    pub delegate: Delegate
}

impl DelegateItem {
    /// Creates a new delegate item
    pub fn new(span: Span, delegate: Delegate) -> Self {
        Self { span, delegate }
    }
}

impl Spanned for DelegateItem {
    fn span(&self) -> Span {
        self.span.span()
    }
}