//! The virtual machine that runs aroma programs

use std::collections::HashMap;
use std::num::NonZero;
use std::sync::Arc;
use std::sync::atomic::{AtomicIsize, AtomicUsize, Ordering};

use parking_lot::{Mutex, RwLock};

use error::VmError;

use crate::chunk::Chunk;
use crate::function::ObjFunction;
use crate::types::Value;
use crate::vm::thread_executor::{
    AromaThreadHandle, AromaThreadId, ThreadExecutor, ThreadResult, ThreadResultHolder,
};

pub mod error;
mod thread_executor;

pub type Chunks = Arc<RwLock<Vec<Arc<Chunk>>>>;

pub type InstructionPointer = (usize, usize);
pub type Globals = Arc<RwLock<HashMap<String, Value>>>;
pub type StaticFunctionTable = Arc<RwLock<HashMap<String, Arc<ObjFunction>>>>;

/// A virtual machine
#[derive(Debug)]
pub struct AromaVm {
    chunks: Chunks,
    next_thread: AtomicUsize,
    thread_results: RwLock<HashMap<AromaThreadId, ThreadResultHolder>>,
    run_control: Arc<AtomicIsize>,
    functions: StaticFunctionTable,
    globals: Globals,
}

impl AromaVm {
    /// Creates a new aroma vm
    pub fn new() -> Self {
        Self {
            chunks: Arc::new(RwLock::new(Default::default())),
            next_thread: AtomicUsize::new(1),
            thread_results: RwLock::new(HashMap::with_capacity(8)),
            run_control: Arc::new(Default::default()),
            functions: Arc::new(Default::default()),
            globals: Arc::new(Default::default()),
        }
    }

    pub fn load(&mut self, mut function: ObjFunction) -> Result<(), VmError> {
        let function_chunks = function.chunks().clone();
        let mut all_chunks = self.chunks.write();
        let start_idx = all_chunks.len();
        function.set_chunk_idx(start_idx);
        all_chunks.extend(function_chunks);
        self.functions
            .write()
            .insert(function.name().to_string(), Arc::new(function));

        Ok(())
    }

    /// Starts the VM by running a static function
    pub fn start(&mut self, name: impl AsRef<str>) -> Result<i32, VmError> {
        let name = name.as_ref().to_string();
        let functino = self
            .functions
            .read()
            .get(&name)
            .ok_or_else(|| VmError::FunctionNotDefined(name))?
            .clone();

        let handle = self.start_thread(functino);
        let id = *handle.id();
        match handle.join() {
            Ok(()) => {
                let guard = self.thread_results.read();
                let r = guard.get(&id).unwrap();
                let result_guard = r.lock();
                let result = result_guard.as_ref().unwrap().as_ref();
                match result {
                    Ok(ThreadResult::Done(code)) => Ok(*code),
                    Ok(ThreadResult::Exception(e)) => {
                        Err(VmError::MainThreadEndedExceptionally(e.clone()))
                    }
                    Err(e) => Err(e.clone()),
                }
            }
            Err(e) => Err(e),
        }
    }

    #[inline]
    fn next_thread_id(&self) -> AromaThreadId {
        AromaThreadId::new(NonZero::new(self.next_thread.fetch_add(1, Ordering::SeqCst)).unwrap())
    }

    /// Starts a thread with a given initial thread
    fn start_thread(&self, function: Arc<ObjFunction>) -> AromaThreadHandle {
        let id = self.next_thread_id();
        let result_mutex = self
            .thread_results
            .write()
            .entry(id)
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone();

        let i = function.chunk_idx().unwrap();
        let handle = ThreadExecutor::start(
            function,
            id,
            self.chunks.clone(),
            self.globals.clone(),
            (i, 0),
            self.run_control.clone(),
            result_mutex,
            self.functions.clone()
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

#[cfg(test)]
mod tests {}
