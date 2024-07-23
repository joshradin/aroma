//! The virtual machine that runs aroma programs

use std::collections::{BTreeMap, HashMap};
use std::num::NonZero;
use std::sync::Arc;
use std::thread;
use gc_arena::{Arena, Collect, Gc, Rootable};
use gc_arena::lock::RefLock;

use crate::chunk::Chunk;
use crate::types::{Obj, Value};

type InstructionPointer = (usize, usize);

/// A virtual machine
pub struct AromaVm {
    chunks: Arc<Vec<Chunk>>,
    threads: Arc<Arena<Rootable![Gc<'_, RefLock<HashMap<AromaThreadId, ThreadExecutor<'_>>>>]>>
}

impl AromaVm {
    /// Creates a new aroma vm
    pub fn new() -> Self {
        Self {
            chunks: Arc::new(vec![]),
            threads: Arc::new(Arena::new(|mc| Gc::new(mc, RefLock::new(HashMap::new())))),
        }
    }

    /// Starts the VM
    pub fn start(&mut self, chunks: Vec<Chunk>) -> Result<(), VmError> {
        self.chunks = Arc::new(chunks);
        self.start_thread(0)
    }

    /// Starts a thread with a given initial thread
    fn start_thread(&self, initial_chunk: usize) -> Result<(), VmError>{

        let threads = self.threads.clone();

        thread::spawn(move || {
            threads.mutate(|mc, roots| {
                let executor = ThreadExecutor {
                    id: AromaThreadId(NonZero::new(1).unwrap()),
                    chunks: Arc::new(vec![]),
                    ip: (initial_chunk, 0),
                    name: None,
                    frame_stack: vec![],
                    state: ThreadState::Dead,
                };
            });
        });






        Ok(())
    }
}



#[derive(Debug, thiserror::Error)]
pub enum VmError {
}


#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct ThreadExecutor<'vm> {
    id: AromaThreadId,
    #[collect(require_static)]
    chunks: Arc<Vec<Chunk>>,
    ip: InstructionPointer,
    name: Option<String>,
    frame_stack: Vec<StackFrame<'vm>>,
    state: ThreadState
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(no_drop)]
pub enum ThreadState {
    Dead
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Collect)]
#[collect(require_static)]
struct AromaThreadId(NonZero<usize>);

#[derive(Debug, Collect)]
#[collect(no_drop)]
struct StackFrame<'vm> {
    stack: Vec<()>,
    vars: BTreeMap<usize, Value<'vm>>,
    pc: usize,
}

pub type ObjectPtr<'gc> = Gc<'gc, RefLock<Obj<'gc>>>;

#[cfg(test)]
mod tests {


}
