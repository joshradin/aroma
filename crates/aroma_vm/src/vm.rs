//! The virtual machine that runs aroma programs

use std::collections::HashMap;
use std::num::NonZero;
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::atomic::{AtomicIsize, AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;

use log::{debug, trace, warn};
use parking_lot::{Mutex, RwLock};

use error::VmError;

use crate::chunk::Chunk;
#[cfg(feature = "jit")]
use crate::jit::{JIT, JITConfig, JitResult};
use crate::types::function::{ObjFunction, ObjNative};
use crate::types::Value;
use crate::vm::natives::NATIVES;
use crate::vm::thread_executor::{
    AromaThreadHandle, AromaThreadId, ThreadExecutor, ThreadResult, ThreadResultHolder,
};

pub mod error;
pub mod natives;
mod thread_executor;

pub type Chunks = Arc<RwLock<Vec<Arc<Chunk>>>>;

pub type InsPtr = (usize, usize);
pub type Globals = Arc<RwLock<HashMap<String, Value>>>;
pub type StaticFunctionTable = Arc<RwLock<HashMap<String, Arc<ObjFunction>>>>;
pub type StaticNativeTable = Arc<RwLock<HashMap<String, Arc<ObjNative>>>>;

/// Configuration used for creating the VM
#[derive(Debug, Default)]
pub struct AromaVmConfig {
    #[cfg(feature = "jit")]
    pub jit: JITConfig
}


/// A virtual machine
#[derive(Debug)]
pub struct AromaVm {
    chunks: Chunks,
    next_thread: AtomicUsize,
    thread_results: RwLock<HashMap<AromaThreadId, ThreadResultHolder>>,
    run_control: Arc<AtomicIsize>,
    functions: StaticFunctionTable,
    natives: StaticNativeTable,
    globals: Globals,
    #[cfg(feature = "jit")]
    jit: Arc<Mutex<JIT>>,
    config: AromaVmConfig
}

impl AromaVm {
    /// Creates a new aroma vm
    #[inline]
    pub fn new() -> Self {
        Self::with_config(AromaVmConfig::default())
    }

    /// Creates a new aroma vm with a given config
    pub fn with_config(aroma_vm_config: AromaVmConfig) -> Self {
        let mut this = Self {
            chunks: Arc::new(RwLock::new(Default::default())),
            next_thread: AtomicUsize::new(1),
            thread_results: RwLock::new(HashMap::with_capacity(8)),
            run_control: Arc::new(Default::default()),
            functions: Arc::new(Default::default()),
            natives: Arc::new(Default::default()),
            globals: Arc::new(Default::default()),
            #[cfg(feature = "jit")]
            jit: Arc::new(Mutex::new(JIT::new())),
            config: aroma_vm_config
        };
        for natives in NATIVES {
            this.add_native(natives).unwrap();
        }
        this
    }

    /// Loads a function into the VM.
    ///
    /// # Info
    /// Functions that have not been loaded into the VM can not be run. This is because
    /// loading functions adds it to the global address space used by the VM.
    pub fn load(&mut self, mut function: ObjFunction) -> Result<(), VmError> {
        let function_chunks = Vec::from(function.chunks());
        let mut all_chunks = self.chunks.write();
        let start_idx = all_chunks.len();
        function.set_chunk_idx(start_idx);
        all_chunks.extend(function_chunks);
        self.functions
            .write()
            .insert(function.name().to_string(), Arc::new(function));

        Ok(())
    }

    /// Adds a native function to this vm
    pub fn add_native(&mut self, native_fn: &ObjNative) -> Result<(), VmError> {
        self.natives
            .write()
            .insert(native_fn.name().to_string(), Arc::new(native_fn.clone()));
        Ok(())
    }

    /// Starts the VM by running a static function
    pub fn start(&mut self, name: impl AsRef<str>) -> Result<i32, VmError> {
        let name = name.as_ref().to_string();
        let function = self
            .functions
            .read()
            .get(&name)
            .ok_or_else(|| VmError::FunctionNotDefined(name))?
            .clone();

        let handle = self.start_thread(function);
        let id = *handle.id();
        #[cfg(feature = "jit")]
        self.jit_thread();
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

    #[cfg(feature = "jit")]
    // TODO: change from this active checking mechanism to a pass feeder system where each thread executor is responsible with sending a threshold passed function to the compiler
    fn jit_thread(&mut self) -> JoinHandle<()> {
        let jit = Arc::downgrade(&self.jit);
        let functions = Arc::downgrade(&self.functions.clone());
        let threshold = self.config.jit.threshold;
        thread::Builder::new()
            .name("JIT Compiler".to_string())
            .spawn(move || {
            trace!("started JIT thread");
            loop {
                if let (Some(jit), Some(functions)) = (jit.upgrade(), functions.upgrade()) {
                    let guard = functions.read();
                    if let Some(func) = guard
                        .values()
                        .find(|i| i.executions() + i.loops() > threshold && i.jit().is_none())
                    {
                        let func = func.clone();
                        drop(guard);
                        debug!(target: "aroma_vm::jit" ,"Starting JIT on {}", func.name());
                        let mut jit = jit.lock();
                        match jit.compile(&*func) {
                            Ok(ptr) => {
                                debug!(target: "aroma_vm::jit", "created JIT compilation for {:?}", ptr);
                                func.set_jit(NonNull::new(ptr as *mut u8).unwrap())
                            }
                            Err(err) => {
                                warn!("Failed to compile {} because {err}", func.name());
                            }
                        }
                    }
                }
            }
        }).expect("could not start JIT thread")
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
            self.functions.clone(),
            self.natives.clone(),
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
