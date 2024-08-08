use std::collections::HashSet;
use std::num::NonZero;
use std::sync::{Arc, RwLock};
use crate::common::identifier::Id;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct CompileJobId(NonZero<>);

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