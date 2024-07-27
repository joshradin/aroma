use std::collections::HashMap;
use aroma_gc::{Gc, Trace};
use crate::chunk::Chunk;
use crate::types::Value;

#[derive(Debug, Trace)]
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
#[derive(Debug, Default, Trace)]
pub struct VTable {
    #[req_static]
    class: String,
    parent: Option<Gc<VTable>>,
    fields: HashMap<Gc<&'static str>, Value>,
    #[req_static]
    methods: HashMap<Gc<&'static str>, Chunk>,
}
