//! Responsible for type resolution

use aroma_ast::id::Id;
use aroma_types::type_signature::TypeSignature;
use std::collections::{HashMap, HashSet};

/// Contains all direct bindings between identifiers and it's type.
#[derive(Debug)]
pub struct Bindings {
    scopes: Vec<Scope>,
}

impl Default for Bindings {
    fn default() -> Self {
        Self::new()
    }
}

impl Bindings {
    /// creates a new bindings
    pub fn new() -> Self {
        let mut b = Self { scopes: vec![] };
        b.new_scope(None);
        b
    }

    /// Increases the current scope
    pub fn new_scope(&mut self, namespace: impl Into<Option<Id>>) {
        self.scopes.push(Scope {
            namespace: namespace.into(),
            types: Default::default(),
            bindings: Default::default(),
        })
    }

    /// Pushes an existing
    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope)
    }

    /// Decreases the current scope, returning the scope that was created.
    ///
    /// Doesn't do anything if current at the global scope
    pub fn pop_scope(&mut self) -> Option<Scope> {
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
        }
    }

    /// Gets the global scope
    pub fn globals(&self) -> &Scope {
        self.scopes.first().unwrap()
    }
    /// Gets the global scope
    pub fn globals_mut(&mut self) -> &mut Scope {
        self.scopes.first_mut().unwrap()
    }

    /// Tries to resolve a type signature by id.
    ///
    /// # Examples
    /// Resolving a locally scoped id
    /// ```
    /// # use aroma_ast::id::Id;
    /// use aroma_ast_parsing::type_resolution::{Bindings, Scope};
    /// use aroma_types::type_signature::TypeSignature;
    /// let mut bindings = Bindings::new();
    /// bindings.new_scope(None);
    /// bindings.insert(Id::new_call_site(["t"]).unwrap(), TypeSignature::Byte);
    /// assert!(matches!(bindings.get(&Id::new_call_site(["t"]).unwrap()), Some((_, TypeSignature::Byte))));
    /// bindings.pop_scope();
    /// assert!(matches!(bindings.get(&Id::new_call_site(["t"]).unwrap()), None));
    /// ```
    pub fn get(&self, id: &Id) -> Option<(Id, &TypeSignature)> {
        for scope in self.scopes.iter().rev() {
            if let Some(sig) = scope.bindings.get(id) {
                return Some((id.clone(), sig));
            } else if let Some(context) = scope.namespace.as_ref() {
                let id = context.concat(id);
                if let Some(sig) = scope.bindings.get(&id) {
                    return Some((id, sig));
                }
            }
        }

        None
    }

    /// Checks if a given id is resolvable in this scope.
    #[inline]
    pub fn contains(&self, id: &Id) -> bool {
        self.get(id).is_some()
    }

    /// Tries to resolve a plain type
    ///
    /// # Examples
    /// Resolving a locally scoped id
    /// ```
    /// # use aroma_ast::id::Id;
    /// use aroma_ast_parsing::type_resolution::{Bindings, Scope};
    /// use aroma_types::type_signature::TypeSignature;
    /// let mut bindings = Bindings::new();
    /// bindings.new_scope(None);
    /// bindings.insert_type(Id::new_call_site(["t"]).unwrap());
    /// assert!(bindings.get_type(&Id::new_call_site(["t"]).unwrap()).is_some());
    /// bindings.pop_scope();
    /// assert!(bindings.get_type(&Id::new_call_site(["t"]).unwrap()).is_none());
    /// ```
    pub fn get_type(&self, id: &Id) -> Option<Id> {
        for scope in self.scopes.iter().rev() {
            if let Some(sig) = scope.types.get(id) {
                return Some(sig.clone());
            } else if let Some(context) = scope.namespace.as_ref() {
                let id = context.concat(id);
                if let Some(id) = scope.types.get(&id) {
                    return Some(id.clone());
                }
            }
        }

        None
    }

    /// Checks if a given id is resolvable in this scope.
    #[inline]
    pub fn contains_type(&self, id: &Id) -> bool {
        self.get_type(id).is_some()
    }

    /// Insert a value into the bindings
    pub fn insert(&mut self, id: Id, value: TypeSignature) {
        let scope = self
            .scopes
            .last_mut()
            .expect("scopes should never be empty");
        scope.bindings.insert(id, value);
    }

    /// Insert a value into the bindings
    pub fn insert_type(&mut self, id: Id) {
        let scope = self
            .scopes
            .last_mut()
            .expect("scopes should never be empty");
        scope.types.insert(id);
    }
}

/// A scope, containing bindings
///
/// This type is opaque, and can only be received from a [Bindings] object.
///
/// # Examples
/// ```
/// # use aroma_ast_parsing::type_resolution::{Bindings, Scope};
/// let mut bindings = Bindings::new();
/// bindings.new_scope(None);
/// let scope: Scope = bindings.pop_scope().unwrap();
/// ```
#[derive(Debug)]
pub struct Scope {
    namespace: Option<Id>,
    types: HashSet<Id>,
    bindings: HashMap<Id, TypeSignature>,
}

#[cfg(test)]
mod tests {
    use crate::type_resolution::Bindings;
    use aroma_ast::id::Id;
    use aroma_types::type_signature::TypeSignature;

    #[test]
    fn test_resolve_type() {
        let mut bindings = Bindings::new();
        let id = Id::new_call_site(["aroma", "system"]).unwrap();
        bindings.insert(
            id.resolve(&Id::new_call_site(["global_value"]).unwrap()),
            TypeSignature::Boolean,
        );
        bindings.new_scope(id.clone());

        println!("{bindings:#?}");

        let bound = bindings
            .get(&id.join("global_value"))
            .expect("could not resolve");
        assert!(matches!(bound, (_, TypeSignature::Boolean)));
    }
}
