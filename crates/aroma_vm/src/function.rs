//! Represents functions

use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;
use crate::chunk::Chunk;
use crate::chunk::OpCode::Pop;
use crate::types::Value;
use crate::vm::error::VmError;

/// A function, an immutable piece of code.
#[derive(Clone)]
pub struct ObjFunction {
    arity: usize,
    chunks: Vec<Arc<Chunk>>,
    chunk_idx: Option<usize>,
    name: String,
}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .finish_non_exhaustive()
    }
}

impl ObjFunction {
    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn chunks(&self) -> &Vec<Arc<Chunk>> {
        &self.chunks
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn new(name: impl AsRef<str>, arity: usize, chunks: Vec<Chunk>) -> Self {
        Self {
            arity,
            chunks: Vec::from_iter(chunks.into_iter().map(|c| Arc::new(c))),
            chunk_idx: None,
            name: name.as_ref().to_string(),
        }
    }

    pub fn chunk_idx(&self) -> Option<usize> {
        self.chunk_idx
    }

    pub fn set_chunk_idx(&mut self, chunk_idx: usize) {
        let _ = self.chunk_idx.insert(chunk_idx);
    }
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialOrd for ObjFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

/// A native function, implemented in rust.
pub type NativeFn = fn(&[Value]) -> Result<Option<Value>, VmError>;

/// A native function.
pub struct ObjNative {
    name: &'static str,
    arity: usize,
    native: NativeFn
}

impl ObjNative {
    /// Creates a new native function
    pub const fn new(name: &'static str, arity: usize, native: NativeFn) -> Self {
        Self { name, arity, native }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn native(&self) -> NativeFn {
        self.native
    }

    pub fn call(&self, values: &[Value]) -> Result<Option<Value>, VmError> {
        (&self.native)(values)
    }
}

impl PartialEq for ObjNative {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialOrd for ObjNative {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}
