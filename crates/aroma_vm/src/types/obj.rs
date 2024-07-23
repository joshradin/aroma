use crate::types::Value;
use gc_arena::Collect;
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Obj<'vm> {
    vtable: VTable<'vm>,
    _lf: PhantomData<&'vm ()>,
}

impl Obj<'_> {
    pub fn new() -> Self {
        Self {
            vtable: VTable::default(),
            _lf: PhantomData,
        }
    }
}

/// The virtual table
#[derive(Debug, Default, Collect)]
#[collect(no_drop)]
pub struct VTable<'vm> {
    class: String,
    parent: Option<Box<VTable<'vm>>>,
    fields: HashMap<String, Value<'vm>>,
    methods: HashMap<String, Vec<u8>>,
    _lf: PhantomData<&'vm ()>,
}
