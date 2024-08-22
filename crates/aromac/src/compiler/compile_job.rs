use aroma_ast::id::Id;
use aroma_ast_parsing::parse_file;
use aroma_ast_parsing::parser::items::TranslationUnit;
use aroma_ast_parsing::parser::SyntaxError;
use log::{debug, info};
use parking_lot::RwLock;
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{sync_channel, Receiver};
use std::sync::Arc;
use std::thread::{Scope, ScopedJoinHandle};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct CompileJobId(NonZeroUsize);

static COMPILE_JOB_ID: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug)]
pub struct CompileJob<'p> {
    id: CompileJobId,
    status: Arc<RwLock<CompileJobStatus<'p>>>,
    receiver: Receiver<CompileJobCommand>,
}

impl<'env> CompileJob<'env> {
    /// Start a compile job
    pub fn start<'scope>(
        scope: &'scope Scope<'scope, 'env>,
        path: &'env Path,
    ) -> CompileJobHandle<'scope, 'env>
    where
        'env: 'scope,
    {
        let status = Arc::new(RwLock::new(CompileJobStatus::NotStarted));
        let id =
            CompileJobId(NonZeroUsize::new(COMPILE_JOB_ID.fetch_add(1, Ordering::SeqCst)).unwrap());
        let (join_handle, tx) = {
            let status = status.clone();
            let (tx, rx) = sync_channel::<CompileJobCommand>(0);

            let handle = scope.spawn(move || {
                let path = path;
                let mut job = CompileJob::new(id, status, rx);
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
            join_handle,
        }
    }

    fn new(
        id: CompileJobId,
        status: Arc<RwLock<CompileJobStatus<'env>>>,
        receiver: Receiver<CompileJobCommand>,
    ) -> Self {
        Self {
            id,
            status,
            receiver,
        }
    }

    fn run(&mut self, path: &'env Path) -> Result<(), CompileError<'env>> {
        *self.status.write() = CompileJobStatus::Parsing;
        let u = parse_file(path)?;
        debug!("compiled {:?} to {} units", path, u.items.len());
        *self.status.write() = CompileJobStatus::Parsed(u);
        loop {
            let finished = {
                let guard = self.status.read();
                matches!(
                    &*guard,
                    CompileJobStatus::Done | CompileJobStatus::Dead | CompileJobStatus::Failed(_)
                )
            };
            if finished {
                break;
            }
            let pass = self.pass()?;
            *self.status.write() = pass;
        }

        Ok(())
    }

    fn pass(&mut self) -> Result<CompileJobStatus<'env>, CompileError<'env>> {
        let status = {
            let mut guard = self.status.write();
            std::mem::replace(&mut *guard, CompileJobStatus::Processing)
        };
        match status {
            CompileJobStatus::Parsed(translation_unit) => {
                info!("items: {translation_unit:#?}");
                todo!()
            }
            state => {
                panic!("can't handle state: {:?}", state)
            }
        }
    }
}

#[derive(Debug)]
pub struct CompileJobHandle<'scope, 'env : 'scope> {
    id: CompileJobId,
    status: Arc<RwLock<CompileJobStatus<'env>>>,
    join_handle: ScopedJoinHandle<'scope, ()>,
}

impl<'scope, 'env : 'scope> CompileJobHandle<'scope, 'env> {
    pub fn id(&self) -> CompileJobId {
        self.id
    }

    pub fn status(&self) -> &Arc<RwLock<CompileJobStatus<'env>>> {
        &self.status
    }

    pub fn is_finished(&self) -> bool {
        self.join_handle.is_finished()
    }

    pub fn take_error(&self) -> Option<CompileError<'env>> {
        let mut guard = self.status.write();
        if matches!(*guard, CompileJobStatus::Failed(_)) {
            let CompileJobStatus::Failed(e) =
                std::mem::replace(&mut *guard, CompileJobStatus::Dead)
            else {
                unreachable!()
            };
            Some(e)
        } else {
            None
        }
    }

    pub fn join(self) -> std::thread::Result<()> {
        self.join_handle.join()
    }
}

/// Compile job status
#[derive(Debug)]
pub enum CompileJobStatus<'env> {
    NotStarted,
    Parsing,
    Processing,
    Parsed(TranslationUnit<'env>),
    WaitingForIdentifiers(TranslationUnit<'env>, HashSet<Id<'env>>),
    Failed(CompileError<'env>),
    Done,
    Dead,
}

#[derive(Debug)]
pub enum CompileJobCommand {
    Cancel,
    UpdatingBindings,
}

/// A compile error
#[derive(Debug)]
pub enum CompileError<'env> {
    Syntax(SyntaxError<'env>),
    UndefinedIdentifiers(Vec<Id<'env>>),
}

impl<'env> From<SyntaxError<'env>> for CompileError<'env> {
    fn from(value: SyntaxError<'env>) -> Self {
        Self::Syntax(value)
    }
}

impl Display for CompileError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Syntax(s) => s.fmt(f),
            CompileError::UndefinedIdentifiers(ids) => {
                write!(f, "Undefined identifiers: {ids:?}")
            }
        }
    }
}

impl Error for CompileError<'_> {}
