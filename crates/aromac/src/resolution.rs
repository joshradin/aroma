//! Resolver for type information and globals

use std::collections::HashMap;
use aroma_ast::id::Id;
use aroma_types::hierarchy::ClassHierarchy;
use aroma_types::type_signature::TypeSignature;

/// Resolver for connecting types and globals
#[derive(Debug)]
pub struct TranslationData {
    class_hierarchy: ClassHierarchy,
    globals: HashMap<Id, TypeSignature>
}


impl TranslationData {
    /// Creates the new translation data instance, with only defaults available
    pub fn new() -> Self {
        Self {
            class_hierarchy: Default::default(),
            globals: Default::default(),
        }
    }

    /// Gets access to the class hierarchy
    pub fn class_hierarchy(&self) -> &ClassHierarchy {
        &self.class_hierarchy
    }

    /// Gets access to the class hierarchy
    pub fn class_hierarchy_mut(&mut self) -> &mut ClassHierarchy {
        &mut self.class_hierarchy
    }

    pub fn globals(&self) -> &HashMap<Id, TypeSignature> {
        &self.globals
    }

    pub fn globals_mut(&mut self) -> &mut HashMap<Id, TypeSignature> {
        &mut self.globals
    }
}

impl Default for TranslationData {
    fn default() -> Self {
        Self::new()
    }
}

