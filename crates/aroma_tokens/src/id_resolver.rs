//! responsible for resolving identifiers

use crate::id::Id;
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct DeclaredId {
    short: Id,
    full: Id,
}

/// An id resolver is responsible with both creating fully qualified identifiers
/// and helping in resolving potentially unqualified Ids into full ones.
#[derive(Debug, Default)]
pub struct IdResolver {
    namespaces: BTreeMap<Id, HashMap<Id, DeclaredId>>,
}

impl IdResolver {
    /// Creates a new, empty id resolver
    pub fn new() -> Self {
        Self {
            namespaces: Default::default(),
        }
    }

    /// Inserts an id that's already considered fully qualified
    pub fn insert_qualified(&mut self, id: Id) {
        let namespace = id.namespace().unwrap_or_default();
        self.build_namespace(namespace).insert(id.most_specific());
    }

    /// Gets a collection of all namespaces in this id resolver
    pub fn namespaces(&self) -> Vec<&Id> {
        self.namespaces.keys().collect()
    }

    /// Creates a namespace builder for adding unqualified ids into a namespace
    pub fn build_namespace(&mut self, namespace: Id) -> NamespaceBuilder {
        self.namespaces.entry(namespace.clone()).or_default();
        NamespaceBuilder {
            resolver: self,
            namespace,
        }
    }

    /// Creates a query object for this resolver
    pub fn query(&self, namespace: Id) -> IdQueries {
        IdQueries {
            resolver: self,
            namespace,
        }
    }

    /// Merges the entries of another id resolver into this id resolver
    pub fn merge(&mut self, other: &Self) {
        for (id, map) in &other.namespaces {
            let mut namespace_map = self.namespaces
                        .entry(id.clone())
                        .or_default();
            for (short, d) in map {
                namespace_map.insert(short.clone(), d.clone());
            }
        }
    }
}

#[derive(Debug)]
pub struct NamespaceBuilder<'a> {
    resolver: &'a mut IdResolver,
    namespace: Id,
}

impl NamespaceBuilder<'_> {
    /// Inserts an unqualified id into this namespace builder, returning a fully qualified id
    pub fn insert(&mut self, id: Id) -> Id {
        let full = self.namespace.resolve(&id);
        let declared = DeclaredId {
            short: id.clone(),
            full: full.clone(),
        };
        self.resolver
            .namespaces
            .get_mut(&self.namespace)
            .unwrap()
            .entry(id)
            .or_insert(declared);
        full
    }

    /// Inserts an unqualified id into this namespace builder that refers to a different fully qualified id. Returns
    /// the original FQI unchanged. This allows for imports into the namespace
    pub fn insert_alias(&mut self, id: Id, fqi: Id) -> Id {
        let declared = DeclaredId {
            short: id.clone(),
            full: fqi.clone(),
        };
        self.resolver
            .namespaces
            .get_mut(&self.namespace)
            .unwrap()
            .entry(id)
            .or_insert(declared);
        fqi
    }

    /// Creates a querier
    pub fn query(&self) -> IdQueries {
        IdQueries {
            resolver: &*self.resolver,
            namespace: self.namespace.clone(),
        }
    }
}

/// Allows fo querying ids
#[derive(Debug)]
pub struct IdQueries<'a> {
    resolver: &'a IdResolver,
    namespace: Id,
}

impl<'a> IdQueries<'a> {
    /// Try to resolve an id query, returning all matching
    pub fn resolve(&self, id: &Id) -> Vec<&'a Id> {
        let mut resolved = vec![];
        // fast path - check directly for this
        let direct_namespace = id.namespace().unwrap_or_default();
        if let Some(valid) = self
            .resolver
            .namespaces
            .get(&direct_namespace)
            .and_then(|map| map.get(&id.most_specific()))
        {
            resolved.push(&valid.full);
        }

        let concat = self.namespace.resolve(id);
        if let Some(namespace) = concat.namespace() {
            if let Some(valid) = self
                .resolver
                .namespaces
                .get(&namespace)
                .and_then(|map| map.get(&id.most_specific()))
            {
                resolved.push(&valid.full);
            }
        }

        resolved
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_build_namespace() {
        let mut id_resolver = IdResolver::new();
        let builder = id_resolver.build_namespace(Id::from_str("std").unwrap());
        assert_eq!(id_resolver.namespaces().len(), 1);
    }

    #[test]
    fn test_resolve_unqualified() {
        let mut id_resolver = IdResolver::new();
        let namespace = Id::from_str("std").unwrap();
        let mut builder = id_resolver.build_namespace(namespace.clone());
        let id = Id::new_call_site(["b"]).unwrap();
        let fqi = builder.insert(id.clone());
        assert_eq!(fqi, Id::new_call_site(["std", "b"]).unwrap());

        let resolved = Vec::from_iter(id_resolver.query(namespace).resolve(&id).iter().cloned());
        assert_eq!(resolved.len(), 1);
    }

    #[test]
    fn test_resolve_qualified() {
        let mut id_resolver = IdResolver::new();
        let namespace = Id::from_str("aroma.system").unwrap();
        let mut builder = id_resolver.build_namespace(namespace.clone());
        let id = Id::new_call_site(["Object"]).unwrap();
        let fqi = builder.insert(id.clone());
        assert_eq!(
            fqi,
            Id::new_call_site(["aroma", "system", "Object"]).unwrap()
        );

        let resolved = id_resolver
            .query(namespace.namespace().unwrap())
            .resolve(&Id::from_str("system.Object").unwrap());
        assert_eq!(resolved.len(), 1);
    }

    #[test]
    fn test_resolve_alias() {
        let mut id_resolver = IdResolver::new();
        let fqi_object = Id::from_str("aroma.system.Object").unwrap();
        id_resolver.insert_qualified(fqi_object.clone());
        let mut namespace_builder =
            id_resolver.build_namespace(Id::from_str("industries.vandaley").unwrap());
        namespace_builder.insert_alias(
            Id::new_call_site(["Object"]).unwrap(), // import Object
            fqi_object.clone(),
        );

        let resolved = namespace_builder
            .query()
            .resolve(&Id::new_call_site(["Object"]).unwrap());

        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0], &fqi_object);
        println!("resolver: {id_resolver:#?}");
    }
}
