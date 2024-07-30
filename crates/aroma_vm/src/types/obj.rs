use crate::chunk::Chunk;
use crate::types::Value;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct Obj {
    vtable: VTable,
}

impl Obj {
    pub fn new() -> Self {
        Self {
            vtable: VTable::default(),
        }
    }
}

/// The virtual table
#[derive(Debug, Default)]
pub struct VTable {
    class: String,
    parent: Option<Arc<VTable>>,
    fields: HashMap<&'static str, Value>,
    methods: HashMap<&'static str, Chunk>,
}
