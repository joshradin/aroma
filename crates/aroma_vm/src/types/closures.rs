use std::sync::Arc;

use crate::types::function::ObjFunction;

/// An object closure,
#[derive(Debug)]
pub struct ObjClosure {
    function: Arc<ObjFunction>,
}

impl ObjClosure {
    /// Creates a new closure object with a base function definition
    pub fn new(func: &Arc<ObjFunction>) -> Self {
        Self {
            function: func.clone(),
        }
    }
}
