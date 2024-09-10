//! General compile job

use super::error::*;
use crate::compiler::compile_job::passes::declaration_discovery::{
    create_declarations, CreateDeclarationError,
};
use crate::compiler::compile_job::passes::fully_qualify::fully_qualify;
use crate::compiler::compile_job::passes::type_check::TypeCheckPass;
use crate::resolution::TranslationData;
use aroma_ast::translation_unit::TranslationUnit;
use aroma_ast::typed::TypeError;
use aroma_ast_parsing::parse_file;
use aroma_ast_parsing::parser::SyntaxError;
use aroma_ast_parsing::type_resolution::Bindings;
use aroma_tokens::id::Id;
use aroma_tokens::id_resolver::{CreateIdError, IdError, IdResolver, ResolveIdError};
use aroma_tokens::spanned::Span;
use aroma_tokens::SpannedError;
use aroma_types::class::Class;
use aroma_types::type_signature::TypeSignature;
use itertools::Itertools as _;
use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::result::Result as StdResult;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc::{sync_channel, Receiver, RecvError, SyncSender, TryRecvError};
use std::sync::Arc;
use std::thread::{Scope, ScopedJoinHandle};
use tracing::{debug, info, instrument, warn};

pub mod passes;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct CompileJobId(NonZeroUsize);

static COMPILE_JOB_ID: AtomicUsize = AtomicUsize::new(1);
pub type Shared<T> = Arc<RwLock<T>>;
pub type SharedJobStatus = Shared<CompileJobStatus>;
pub type SharedBindings = Shared<HashMap<Id, TypeData>>;

#[derive(Debug)]
pub struct CompileJob {
    paused: bool,
    id: CompileJobId,
    status: SharedJobStatus,
    bindings: SharedBindings,
    state: Option<CompileJobState>,
    receiver: Receiver<CompileJobCommand>,
}

impl CompileJob {
    /// Start a compile job
    pub fn start<'scope>(
        span: tracing::Span,
        scope: &'scope Scope<'scope, '_>,
        path: &'scope Path,
        is_paused: bool,
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
                let mut job = CompileJob::new(id, status, bindings, rx, is_paused);
                match span.in_scope(|| job.run(path)) {
                    Ok(()) => {
                        *job.status.write() = CompileJobStatus::Done;
                    }
                    Err(err) => {
                        warn!("job finally failed: {err}");
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
        paused: bool,
    ) -> Self {
        Self {
            paused,
            id,
            status,
            bindings,
            state: None,
            receiver,
        }
    }

    #[instrument(skip(self), name = "job", fields(id=?self.id))]
    fn run(&mut self, path: &Path) -> StdResult<(), CompileError> {
        debug!("started running compile job for {:?}", path);
        self.state = Some(CompileJobState::NotStarted(path.to_path_buf()));

        loop {
            let command = match self.receiver.try_recv() {
                Ok(command) => Some(command),
                Err(TryRecvError::Empty) => None,
                Err(TryRecvError::Disconnected) => {
                    break;
                }
            };
            if let Some(command) = command {
                debug!("got command: {command:?}");
                match command {
                    CompileJobCommand::Cancel => return Err(CompileErrorKind::Cancelled.into()),
                    CompileJobCommand::UpdatingBindings(bindings) => {
                        self.update_bindings(bindings)?;
                    }
                    CompileJobCommand::Pause => {
                        self.paused = true;
                    }
                    CompileJobCommand::Resume => {
                        self.paused = false;
                    }
                }
            };

            if self.paused {
                debug!("paused");
                while self.paused {
                    let t = self.receiver.recv()?;
                    match t {
                        CompileJobCommand::Resume => {
                            self.paused = false;
                        }
                        CompileJobCommand::Cancel => {
                            return Err(CompileErrorKind::Cancelled.into());
                        }
                        CompileJobCommand::UpdatingBindings(bindings) => {
                            self.update_bindings(bindings)?;
                        }
                        _ => {}
                    }
                }
                debug!("resumed");
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

    fn update_bindings(&mut self, bindings: HashMap<Id, TypeData>) -> std::result::Result<(), CompileError>{
        debug!("updating bindings...");
        let mut td = match &mut self.state {
            Some(CompileJobState::WaitingForIdentifiers(pass, waiting)) => {
                pass.data_mut()
            }
            _ => panic!("must be in waiting for identifiers state"),
        };
        for (id, binding) in bindings {
            match binding {
                TypeData::Class(class) => {
                    td.insert_class(&class)?;
                }
                TypeData::Global(_) => {}
            }
        }
        Ok(())
    }

    fn pass(&mut self) -> StdResult<(), CompileError> {
        if let Some(state) = self.state.take() {
            let next_state: Option<CompileJobState> = match state {
                CompileJobState::Done => None,
                CompileJobState::Parsed(unit) => Some(fully_qualify(unit)?),
                CompileJobState::FullyQualified(tu, data) => Some(create_declarations(tu, data)?),
                CompileJobState::IdentifiersCreated(unit, created, ids) => {
                    debug!("compile job {:?} created ids: {ids:#?}", self.id);
                    self.bindings.write().extend(ids);
                    let pass = TypeCheckPass::new(unit, created);
                    Some(pass.pass()?)
                }
                CompileJobState::WaitingForIdentifiers(pass, waiting) => {
                    Some(CompileJobState::WaitingForIdentifiers(pass, waiting))
                }
                CompileJobState::NotStarted(path) => {
                    let Some(u) = parse_file(&path)? else {
                        debug!("got empty translation unit, skipping to end");
                        return Ok(());
                    };
                    debug!("compiled {:?} to {} units", path, u.items.len());
                    Some(CompileJobState::Parsed(u))
                }
                CompileJobState::TypesChecked(_, _) => None,
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
            Err(CompileErrorKind::NoState.into())
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
    TypesChecked,
    WaitingForIdentifiers(HashSet<Id>),
    Failed(CompileError),
    Done,
}

/// Compile job state
#[derive(Debug)]
enum CompileJobState {
    NotStarted(PathBuf),
    Parsed(TranslationUnit),
    IdentifiersCreated(TranslationUnit, TranslationData, HashMap<Id, TypeData>),
    FullyQualified(TranslationUnit, TranslationData),
    TypesChecked(TranslationUnit, TranslationData),
    WaitingForIdentifiers(TypeCheckPass, HashSet<Id>),
    Done,
}

#[derive(Debug, Clone)]
pub enum TypeData {
    Class(Class),
    Global(TypeSignature),
}

impl CompileJobState {
    fn status(&self) -> CompileJobStatus {
        use CompileJobStatus::*;
        match self {
            CompileJobState::Parsed(_) => Parsing,
            CompileJobState::FullyQualified(_, _) => FullyQualified,
            CompileJobState::WaitingForIdentifiers(_, ids) => WaitingForIdentifiers(ids.clone()),
            CompileJobState::Done => Done,
            CompileJobState::IdentifiersCreated(_, _, _) => IdentifiersCreated,
            CompileJobState::NotStarted(_) => NotStarted,
            CompileJobState::TypesChecked(_, _) => TypesChecked,
        }
    }
}

#[derive(Debug)]
pub enum CompileJobCommand {
    Pause,
    Resume,
    Cancel,
    UpdatingBindings(HashMap<Id, TypeData>),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct CompileError(#[from] SpannedError<CompileErrorKind, CompileError>);

impl CompileError {
    pub fn new(
        error: impl Into<CompileErrorKind>,
        span: impl Into<Option<Span>>,
        cause: impl Into<Option<Self>>,
    ) -> Self {
        Self(SpannedError::new(error.into(), span, cause))
    }
}

/// A compile error
#[derive(Debug, thiserror::Error)]
pub enum CompileErrorKind {
    #[error(transparent)]
    Syntax(#[from] SyntaxError),
    #[error(transparent)]
    CreateIdentifier(#[from] CreateDeclarationError),
    #[error(transparent)]
    ResolveIdError(#[from] ResolveIdError),
    #[error(transparent)]
    CreateIdError(#[from] CreateIdError),
    #[error(transparent)]
    TypeError(#[from] aroma_types::hierarchy::Error),
    #[error("undefined identifiers: {0}")]
    UndefinedIdentifier(Id),
    #[error("no state")]
    NoState,
    #[error("job was cancelled")]
    Cancelled,
    #[error("Compile job daemon disconnected")]
    Disconnected(#[from] RecvError),
    #[error("{}", .0.iter().join("\n"))]
    Multi(Vec<CompileError>),
}

impl<V: Into<CompileErrorKind>> From<V> for CompileError {
    fn from(value: V) -> Self {
        CompileError::from(SpannedError::new(value.into(), None, None))
    }
}

impl From<Vec<CompileError>> for CompileErrorKind {
    fn from(value: Vec<CompileError>) -> Self {
        Self::Multi(value)
    }
}

impl From<IdError> for CompileErrorKind {
    fn from(value: IdError) -> Self {
        match value {
            IdError::Resolve(r) => Self::from(r),
            IdError::Create(c) => Self::from(c),
        }
    }
}
