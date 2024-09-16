//! Resolver for type information and globals

use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::IdResolver;
use aroma_types::class::Class;
use aroma_types::hierarchy::Hierarchy;
use aroma_types::type_signature::TypeSignature;
use std::collections::HashMap;

/// Resolver for connecting types and globals
#[derive(Debug)]
pub struct TranslationData {
    namespace: Option<Id>,
    id_resolver: IdResolver,
    class_hierarchy: Hierarchy,
    globals: HashMap<Id, TypeSignature>,
}

impl TranslationData {
    /// Creates the new translation data instance, with only defaults available
    pub fn new() -> Self {
        let hierarchy: Hierarchy = Hierarchy::new();
        let mut resolver: IdResolver = IdResolver::new();
        for class in hierarchy.classes() {
            resolver.insert_qualified(class.id().clone()).expect("could not insert class");
            resolver
                .build_namespace(Id::default())
                .insert_alias(class.id().most_specific(), class.id().clone())
                .expect("could not insert alias");
        }

        Self {
            namespace: None,
            id_resolver: resolver,
            class_hierarchy: hierarchy,
            globals: Default::default(),
        }
    }

    pub fn set_namespace(&mut self, id: Id) {
        self.namespace = Some(id);
    }

    pub fn id_resolver(&self) -> &IdResolver {
        &self.id_resolver
    }
    pub fn id_resolver_mut(&mut self) -> &mut IdResolver {
        &mut self.id_resolver
    }

    /// Tries to insert a class into this translation data
    pub fn insert_class(&mut self, class: &Class) -> aroma_types::hierarchy::Result<()> {
        let class = class.clone();
        let queries = self
            .id_resolver
            .query(self.namespace.as_ref().cloned().unwrap_or_default());
        self.class_hierarchy.insert(class)?;
        Ok(())
    }

    /// Merges the contents of another translation into this
    ///
    /// # Examples
    /// ```
    /// # use aromac::resolution::TranslationData;
    /// let mut td = TranslationData::new();
    /// td.merge(&TranslationData::new()).merge(&TranslationData::new());
    /// ```
    pub fn merge(&mut self, other: &Self) -> &mut Self {
        self
    }

    pub fn class_hierarchy(&self) -> &Hierarchy {
        &self.class_hierarchy
    }

    pub fn globals(&self) -> &HashMap<Id, TypeSignature> {
        &self.globals
    }
}

impl Default for TranslationData {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::resolution::TranslationData;
    use aroma_tokens::id::Id;
    use aroma_types::hierarchy::intrinsics::BASE_CLASS_NAME;
    use std::str::FromStr;

    #[test]
    fn test_intrinsic_classes_alias() {
        let data = TranslationData::new();
        let resolved = data
            .id_resolver()
            .query(Id::from_str("simply.package").unwrap())
            .resolve(&Id::from_str("Object").unwrap())
            .unwrap();
        assert_eq!(resolved.to_string(), BASE_CLASS_NAME);
    }
}
