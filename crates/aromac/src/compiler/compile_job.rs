//! General compile job

use super::error::*;
use crate::compiler::compile_job::passes::declaration_discovery::{
    create_declarations, CreateIdentifierError,
};
use crate::resolution::TranslationData;
use aroma_tokens::id::Id;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast_parsing::parse_file;
use aroma_ast_parsing::parser::SyntaxError;
use aroma_ast_parsing::type_resolution::Bindings;
use itertools::Itertools as _;
use log::{debug, info};
use parking_lot::RwLock;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::path::Path;
use std::result::Result as StdResult;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::Arc;
use std::thread::{Scope, ScopedJoinHandle};
use aroma_tokens::id_resolver::IdResolver;
use crate::compiler::compile_job::passes::fully_qualify::fully_qualify;

pub mod passes;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct CompileJobId(NonZeroUsize);

static COMPILE_JOB_ID: AtomicUsize = AtomicUsize::new(1);
pub type Shared<T> = Arc<RwLock<T>>;
pub type SharedJobStatus = Shared<CompileJobStatus>;
pub type SharedBindings = Shared<Bindings>;

#[derive(Debug)]
pub struct CompileJob {
    id: CompileJobId,
    status: SharedJobStatus,
    bindings: SharedBindings,
    state: Option<CompileJobState>,
    receiver: Receiver<CompileJobCommand>,
}

impl CompileJob {
    /// Start a compile job
    pub fn start<'scope>(
        scope: &'scope Scope<'scope, '_>,
        path: &'scope Path,
    ) -> CompileJobHandle<'scope> {
        let status = Arc::new(RwLock::new(CompileJobStatus::NotStarted));
        let bindings = SharedBindings::default();
        let id =
            CompileJobId(NonZeroUsize::new(COMPILE_JOB_ID.fetch_add(1, Ordering::SeqCst)).unwrap());
        let (join_handle, tx) = {
            let status = status.clone();
            let bindings = bindings.clone();
            let (tx, rx) = sync_channel::<CompileJobCommand>(0);

            let handle = scope.spawn(move || {
                let path = path;
                let mut job = CompileJob::new(id, status, bindings, rx);
                match job.run(path) {
                    Ok(()) => {
                        *job.status.write() = CompileJobStatus::Done;
                    }
                    Err(err) => {
                        *job.status.write() = CompileJobStatus::Failed(err);
                    }
                }
            });
            (handle, tx)
        };
        CompileJobHandle {
            id,
            status,
            bindings,
            sender: tx,
            join_handle,
        }
    }

    fn new(
        id: CompileJobId,
        status: SharedJobStatus,
        bindings: SharedBindings,
        receiver: Receiver<CompileJobCommand>,
    ) -> Self {
        Self {
            id,
            status,
            bindings,
            state: None,
            receiver,
        }
    }

    fn run(&mut self, path: &Path) -> StdResult<(), CompileError> {
        *self.status.write() = CompileJobStatus::Parsing;
        let u = parse_file(path)?;
        // debug!("compiled {:?} to {} units", path, u.items.len());
        self.state = Some(CompileJobState::Parsed(u));
        *self.status.write() = CompileJobStatus::Parsed;

        loop {
            if let Some(command) = self.receiver.try_recv().ok() {
                debug!("got command: {command:?}");
                match command {
                    CompileJobCommand::Cancel => return Err(CompileError::Cancelled),
                    CompileJobCommand::UpdatingBindings => {}
                }
            }

            let finished = {
                let guard = self.status.read();
                matches!(
                    &*guard,
                    CompileJobStatus::Done | CompileJobStatus::Failed(_)
                )
            };
            if finished {
                break;
            }
            self.pass()?;
        }

        Ok(())
    }

    fn pass(&mut self) -> StdResult<(), CompileError> {
        if let Some(state) = self.state.take() {
            let next_state: Option<CompileJobState> = match state {
                CompileJobState::Done => None,
                CompileJobState::Parsed(unit) => Some(fully_qualify(unit)?),
                CompileJobState::IdentifiersCreated(unit, created) => {
                    todo!()
                }
                CompileJobState::WaitingForIdentifiers(_, _, _) => {
                    todo!()
                }
                CompileJobState::FullyQualified(_, _) => {
                    todo!()
                }
            };
            if let Some(next_state) = next_state {
                *self.status.write() = next_state.status();
                self.state = Some(next_state);
            } else {
                *self.status.write() = CompileJobStatus::Done;
                self.state = None;
            }
            Ok(())
        } else {
            Err(CompileError::NoState)
        }
    }
}

#[derive(Debug)]
pub struct CompileJobHandle<'scope> {
    id: CompileJobId,
    status: SharedJobStatus,
    bindings: SharedBindings,
    sender: SyncSender<CompileJobCommand>,
    join_handle: ScopedJoinHandle<'scope, ()>,
}

impl<'scope> CompileJobHandle<'scope> {
    pub fn id(&self) -> CompileJobId {
        self.id
    }

    pub fn status(&self) -> &SharedJobStatus {
        &self.status
    }

    pub fn bindings(&self) -> &SharedBindings {
        &self.bindings
    }

    pub fn is_finished(&self) -> bool {
        self.join_handle.is_finished()
    }

    pub fn take_error(&self) -> Option<CompileError> {
        let mut guard = self.status.write();
        if matches!(*guard, CompileJobStatus::Failed(_)) {
            let CompileJobStatus::Failed(e) =
                std::mem::replace(&mut *guard, CompileJobStatus::Done)
            else {
                unreachable!()
            };
            Some(e)
        } else {
            None
        }
    }

    pub fn send(&self, compile_job_command: CompileJobCommand) -> Result<()> {
        Ok(self.sender.send(compile_job_command)?)
    }

    pub fn join(self) -> std::thread::Result<()> {
        self.join_handle.join()
    }
}

/// Compile job status
#[derive(Debug)]
pub enum CompileJobStatus {
    NotStarted,
    Parsing,
    Processing,
    Parsed,
    IdentifiersCreated,
    FullyQualified,
    WaitingForIdentifiers(HashSet<Id>),
    Failed(CompileError),
    Done,
}

/// Compile job state
#[derive(Debug)]
enum CompileJobState {
    Parsed(TranslationUnit),
    IdentifiersCreated(TranslationUnit, TranslationData),
    FullyQualified(TranslationData, TranslationData),
    WaitingForIdentifiers(TranslationUnit, TranslationData, HashSet<Id>),
    Done,
}

impl CompileJobState {
    fn status(&self) -> CompileJobStatus {
        use CompileJobStatus::*;
        match self {
            CompileJobState::Parsed(_) => Parsing,
            CompileJobState::FullyQualified(_, _) => FullyQualified,
            CompileJobState::WaitingForIdentifiers(_, _, ids) => WaitingForIdentifiers(ids.clone()),
            CompileJobState::Done => Done,
            CompileJobState::IdentifiersCreated(_, _) => IdentifiersCreated,
        }
    }
}

#[derive(Debug)]
pub enum CompileJobCommand {
    Cancel,
    UpdatingBindings,
}

/// A compile error
#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),
    #[error(transparent)]
    CreateIdentifier(#[from] CreateIdentifierError),
    #[error("undefined identifiers: {}", .0.iter().map(|id| id.to_string()).join(","))]
    UndefinedIdentifiers(Vec<Id>),
    #[error("no state")]
    NoState,
    #[error("job was cancelled")]
    Cancelled,
}
