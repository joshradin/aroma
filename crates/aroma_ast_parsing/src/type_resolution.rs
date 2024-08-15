//! Responsible for type resolution

use aroma_ast::id::Id;
use std::collections::HashMap;

/// Contains all direct bindings between identifiers and it's type.
#[derive(Debug)]
pub struct Bindings<'p> {
    bindings: HashMap<Id<'p>, String>,
}
