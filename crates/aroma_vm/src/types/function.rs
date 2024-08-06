//! Represents functions

use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::ptr::{NonNull, null_mut};
use std::sync::{Arc, atomic};
use std::sync::atomic::{AtomicPtr, AtomicUsize};
use itertools::Itertools;
use aroma_bytecode::chunk::Chunk;
use crate::types::{Type, Value};
use crate::vm::error::VmError;

/// A function, an immutable piece of code.
#[derive(Clone)]
pub struct ObjFunction {
    name: String,
    params_ty: Box<[Type]>,
    return_ty: Option<Type>,
    variables: Box<[Type]>,
    chunks: Vec<Arc<Chunk>>,
    chunk_idx: Option<usize>,
    #[cfg(feature = "jit")]
    jit: Arc<AtomicPtr<u8>>,
    /// tracks how many times this function has been called
    executions: Arc<AtomicUsize>,
    /// tracks how many times this function has looped
    loops: Arc<AtomicUsize>,
}

impl Debug for ObjFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("params", &self.params_ty)
            .field("ret", &self.return_ty)
            .finish_non_exhaustive()
    }
}

impl ObjFunction {
    /// Creates a new [ObjFunction] with a given name, parameters, return type, and variables.
    pub fn new(
        name: impl AsRef<str>,
        params: &[Type],
        ret_type: Option<&Type>,
        variables: &[Type],
        chunks: Vec<Chunk>,
    ) -> Self {
        Self {
            name: name.as_ref().to_string(),
            chunks: Vec::from_iter(chunks.into_iter().map(|c| Arc::new(c))),
            chunk_idx: None,
            params_ty: Vec::from(params).into_boxed_slice(),
            return_ty: ret_type.cloned(),
            variables: Vec::from(variables).into_boxed_slice(),
            #[cfg(feature = "jit")]
            jit: Arc::new(Default::default()),
            executions: Arc::new(Default::default()),
            loops: Arc::new(Default::default()),
        }
    }

    /// The number of parameters for this function
    pub fn arity(&self) -> usize {
        self.params_ty.len()
    }

    /// Gets access to the chunks that make up this function
    pub fn chunks(&self) -> &[Arc<Chunk>] {
        &self.chunks
    }

    /// The name of the function
    pub fn name(&self) -> &str {
        &self.name
    }


    pub(crate) fn chunk_idx(&self) -> Option<usize> {
        self.chunk_idx
    }

    pub(crate) fn set_chunk_idx(&mut self, chunk_idx: usize) {
        let _ = self.chunk_idx.insert(chunk_idx);
    }

    /// The parameter types
    pub fn params_ty(&self) -> &[Type] {
        &*self.params_ty
    }

    /// the return types
    pub fn return_ty(&self) -> Option<&Type> {
        self.return_ty.as_ref()
    }

    pub(crate) fn mark_executed(&self) {
        self.executions.fetch_add(1, atomic::Ordering::Relaxed);
    }

    pub(crate) fn executions(&self) -> usize {
        self.executions.load(atomic::Ordering::Relaxed)
    }

    pub(crate) fn mark_looped(&self) {
        self.loops.fetch_add(1, atomic::Ordering::Relaxed);
    }

    pub(crate) fn loops(&self) -> usize {
        self.loops.load(atomic::Ordering::Relaxed)
    }

    /// gets a pointer to the JIT compiled code, if present.
    #[cfg(feature = "jit")]
    pub(crate) fn jit(&self) -> Option<NonNull<u8>> {
        let loaded = self.jit.load(atomic::Ordering::Relaxed);
        NonNull::new(loaded)
    }

    /// Sets the JIT code for this function, if not already present
    #[cfg(feature = "jit")]
    pub(crate) fn set_jit(&self, jit: NonNull<u8>) {
        let as_ptr = jit.as_ptr();
        let _ = self.jit.compare_exchange(
            null_mut(),
            as_ptr,
            atomic::Ordering::Acquire,
            atomic::Ordering::Relaxed,
        );
    }

    /// Gets the signature of this function
    pub fn signature(&self) -> FnSignature {
        FnSignature {
            input: self.params_ty.clone(),
            output: self.return_ty.as_ref().map(|s| Box::new(s.clone())),
        }
    }

    pub fn variables(&self) -> &[Type] {
        &self.variables
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
#[derive(Debug, Clone)]
pub struct ObjNative {
    name: &'static str,
    arity: usize,
    native: NativeFn,
}

impl ObjNative {
    /// Creates a new native function
    pub const fn new(name: &'static str, arity: usize, native: NativeFn) -> Self {
        Self {
            name,
            arity,
            native,
        }
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

/// A signature for a function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
    pub input: Box<[Type]>,
    pub output: Option<Box<Type>>,
}

impl FnSignature {
    pub fn parameters(&self) -> &[Type] {
        &self.input
    }

    pub fn ret(&self) -> Option<&Type> {
        self.output.as_ref().map(|s| &**s)
    }
}

impl Display for FnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        write!(f, "{}", self.parameters().iter().map(|s| format!("{s}")).join(", "))?;
        write!(f, ")")?;
        if let Some(ret) = self.ret() {
            write!(f, " -> {ret}")?;
        }
        Ok(())
    }
}