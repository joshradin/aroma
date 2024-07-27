//! The virtual machine that runs aroma programs

use std::collections::{BTreeMap, HashMap};
use std::num::NonZero;
use std::sync::Arc;
use std::thread;

use crate::chunk::Chunk;
use crate::types::{Obj, Value};

type InstructionPointer = (usize, usize);

/// A virtual machine
pub struct AromaVm {
    chunks: Arc<Vec<Chunk>>,
}

impl AromaVm {
    /// Creates a new aroma vm
    pub fn new() -> Self {
        Self {
            chunks: Arc::new(vec![]),

        }
    }

    /// Starts the VM
    pub fn start(&mut self, chunks: Vec<Chunk>) -> Result<(), VmError> {
        self.chunks = Arc::new(chunks);
        self.start_thread(0)
    }

    /// Starts a thread with a given initial thread
    fn start_thread(&self, initial_chunk: usize) -> Result<(), VmError> {
        todo!();

        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VmError {}

#[derive(Debug)]
pub struct ThreadExecutor {
    id: AromaThreadId,
    chunks: Arc<Vec<Chunk>>,
    ip: InstructionPointer,
    name: Option<String>,
    frame_stack: Vec<StackFrame>,
    state: ThreadState,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ThreadState {
    Dead,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct AromaThreadId(NonZero<usize>);

#[derive(Debug)]
struct StackFrame {
    stack: Vec<()>,
    vars: BTreeMap<usize, Value>,
    pc: usize,
}

pub type ObjectPtr = aroma_gc::Gc<Obj>;


#[cfg(test)]
mod tests {}
