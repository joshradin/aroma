use aroma_ast::identifier::Id;
use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::sync::{Arc, RwLock};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct CompileJobId(NonZeroUsize);

#[derive(Debug)]
pub struct CompileJob {
    status: Arc<RwLock<CompileJobStatus>>
}

#[derive(Debug)]
pub struct CompileJobHandle {
    status: Arc<RwLock<CompileJobStatus>>
}

/// Compile job status
#[derive(Debug)]
pub enum CompileJobStatus {
    Compiling,
    WaitingForIdentifiers(HashSet<Id<'static>>),
    Failed,
}

#[derive(Debug)]
pub enum CompileJobCommand {
    Cancel,
    UpdatingBindings
}