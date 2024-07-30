//! Represents functions

use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use crate::chunk::Chunk;

/// A function, an immutable piece of code.
#[derive(Clone)]
pub struct Function {
    arity: usize,
    chunks: Vec<Chunk>,
    chunk_idx: Option<usize>,
    name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .finish_non_exhaustive()
    }
}

impl Function {
    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn chunks(&self) -> &Vec<Chunk> {
        &self.chunks
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn new(name: impl AsRef<str>, arity: usize, chunks: Vec<Chunk>) -> Self {
        Self {
            arity,
            chunks,
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

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}