//! The virtual machine that runs aroma programs

use std::collections::{BTreeMap, HashMap};
use std::num::NonZero;
use std::sync::Arc;
use std::sync::atomic::{AtomicIsize, AtomicUsize, Ordering};
use std::thread::JoinHandle;

use parking_lot::{Mutex, RwLock};

use crate::chunk::Chunk;
use crate::types::Value;

type InstructionPointer = (usize, usize);
type ThreadResultHolder = Arc<Mutex<Option<Result<ThreadResult, VmError>>>>;

/// A virtual machine
pub struct AromaVm {
    chunks: Arc<Vec<Chunk>>,
    next_thread: AtomicUsize,
    thread_results: RwLock<HashMap<AromaThreadId, ThreadResultHolder>>,
    run_control: Arc<AtomicIsize>,
}

impl AromaVm {
    /// Creates a new aroma vm
    pub fn new() -> Self {
        Self {
            chunks: Arc::new(vec![]),
            next_thread: AtomicUsize::new(1),
            thread_results: RwLock::new(HashMap::with_capacity(8)),
            run_control: Arc::new(Default::default()),
        }
    }

    /// Starts the VM
    pub fn start(&mut self, chunks: Vec<Chunk>) -> Result<(), VmError> {
        self.chunks = Arc::new(chunks);
        let handle = self.start_thread(0);
        match handle.join() {
            Ok(()) => Ok(()),
            Err(e) => Err(e),
        }
    }

    #[inline]
    fn next_thread_id(&self) -> AromaThreadId {
        AromaThreadId(NonZero::new(self.next_thread.fetch_add(1, Ordering::SeqCst)).unwrap())
    }

    /// Starts a thread with a given initial thread
    fn start_thread(&self, initial_chunk: usize) -> AromaThreadHandle {
        let id = self.next_thread_id();
        let result_mutex = self
            .thread_results
            .write()
            .entry(id)
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone();

        let handle = ThreadExecutor::start(
            id,
            self.chunks.clone(),
            (initial_chunk, 0),
            self.run_control.clone(),
            result_mutex,
        );
        handle
    }

    fn get_thread_result(
        &self,
        aroma_thread_id: &AromaThreadId,
    ) -> Option<Result<ThreadResult, VmError>> {
        self.thread_results
            .read()
            .get(aroma_thread_id)
            .and_then(|holder| holder.lock().clone())
    }
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum VmError {
    #[error("Main thread ended in exception {0:?}")]
    MainThreadEndedExceptionally(Value),
    #[error("{}", .0.as_ref().map(|p| format!("Thread Panicked: {p}")).unwrap_or_else(|| "Thead Panicked".to_string()))]
    ThreadPanicked(Option<String>),
}

/// Thread executor
#[derive(Debug)]
pub struct ThreadExecutor {
    id: AromaThreadId,
    chunks: Arc<Vec<Chunk>>,
    ip: InstructionPointer,
    name: Option<String>,
    frame_stack: Vec<StackFrame>,
    state: ThreadState,

    vm_run_control: Arc<AtomicIsize>,
    thread_run_control: Arc<AtomicIsize>,
}

impl ThreadExecutor {
    fn start(
        id: AromaThreadId,
        chunks: Arc<Vec<Chunk>>,
        ip: InstructionPointer,
        vm_run_control: Arc<AtomicIsize>,
        result_mutex: ThreadResultHolder,
    ) -> AromaThreadHandle {
        let thread_controller = Arc::new(AtomicIsize::new(0));
        let mut this = Self {
            id,
            chunks,
            ip,
            name: None,
            frame_stack: vec![],
            state: ThreadState::Dead,
            vm_run_control,
            thread_run_control: thread_controller.clone(),
        };
        let handle = {
            std::thread::spawn(move || {
                let ret = this.run();
                *result_mutex.lock() = Some(ret);
            })
        };
        AromaThreadHandle {
            handle,
            thread_run_control: thread_controller,
        }
    }

    fn run(&mut self) -> Result<ThreadResult, VmError> {
        todo!()
    }
}

#[derive(Debug)]
struct AromaThreadHandle {
    handle: JoinHandle<()>,
    thread_run_control: Arc<AtomicIsize>,
}

impl AromaThreadHandle {
    pub fn join(self) -> Result<(), VmError> {
        self.thread_run_control.store(-1, Ordering::SeqCst);
        self.handle.join().map_err(|e| {
            VmError::ThreadPanicked(e.downcast_ref::<&str>().map(|s| s.to_string()))
        })?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ThreadResult {
    Done,
    Exception(Value),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ThreadState {
    Dead,
    Running,
    Waiting,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct AromaThreadId(NonZero<usize>);

#[derive(Debug)]
struct StackFrame {
    stack: Vec<()>,
    vars: BTreeMap<usize, Value>,
    pc: usize,
}

#[cfg(test)]
mod tests {}
