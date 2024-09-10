//! Responsible with compiling aroma files into aroma object files

use crate::compiler::compile_job::{
    CompileError, CompileJob, CompileJobCommand, CompileJobHandle, CompileJobId, CompileJobStatus,
};
use error::AromaCError;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::panic::resume_unwind;
use std::path::{Path, PathBuf};
use std::{io, thread};
use thiserror::Error;
use tracing::{debug, error, error_span, info, instrument, trace_span, warn};

mod compile_job;
pub mod error;

/// Responsible with compiling aroma files into aroma object files.
///
/// Must be configured using an [AromaCBuilder].
#[derive(Debug)]
pub struct AromaC {
    max_jobs: usize,
    output_directory: PathBuf,
}

impl AromaC {
    /// Creates the default AromaCBuilder
    #[inline]
    pub fn builder() -> AromaCBuilder {
        AromaCBuilder::new()
    }

    /// Compile a file at a given path
    #[inline]
    pub fn compile(&mut self, path: &Path) -> Result<(), AromaCError> {
        self.compile_all([path])
    }

    /// Compile a file at a given path
    pub fn compile_all<'p, I>(&mut self, paths: I) -> Result<(), AromaCError>
    where
        I: IntoIterator<Item = &'p Path>,
    {
        let span = error_span!("compiling");
        span.in_scope(|| {
            let paths = paths.into_iter().collect::<HashSet<&'p Path>>();
            let mut errors = thread::scope::<'p, _, _>(|scope| {
                let mut errors: Vec<AromaCError> = Default::default();
                let mut running_jobs: HashMap<CompileJobId, CompileJobHandle> = HashMap::new();
                let mut paused_jobs: HashMap<CompileJobId, CompileJobHandle> = HashMap::new();
                for path in paths {
                    debug!("creating compile job for {path:?}");
                    let job = CompileJob::start(span.clone(), scope, path, true);
                    paused_jobs.insert(job.id(), job);
                }
                while !(running_jobs.is_empty() && paused_jobs.is_empty()) {
                    while running_jobs.len() < self.max_jobs && !paused_jobs.is_empty() {
                        if let Some(next) = Self::select_next(&mut paused_jobs) {
                            let handle = paused_jobs.remove(&next).unwrap();
                            debug!("resuming {next:?}");
                            if let Err(e) = handle.send(CompileJobCommand::Resume) {
                                errors.push(AromaCError::from(e));
                            } else {
                                running_jobs.insert(next.clone(), handle);
                            }
                        } else {
                            warn!("no resume-able jobs found!");
                            if running_jobs.is_empty() {
                                errors.push(
                                    AromaCError::AllJobsPaused
                                );
                                return errors;
                            }
                            break;
                        }
                    }

                    let mut completed = HashSet::<CompileJobId>::new();
                    let mut to_pause = HashSet::<CompileJobId>::new();
                    for job in running_jobs.values() {
                        let status = job.status().read();
                        match *status {
                            CompileJobStatus::Failed(ref f) => {
                                error!("job {:?} failed: {f}", job.id());
                                completed.insert(job.id());
                            }
                            CompileJobStatus::WaitingForIdentifiers(ref ids) => {
                                warn!("found job {:?} waiting for identifiers: {ids:?}", job.id());
                                to_pause.insert(job.id());
                            }
                            CompileJobStatus::Done => {
                                completed.insert(job.id());
                            }
                            _ => {}
                        }

                        if job.is_finished() {
                            info!("job {:?} thread finished", job.id());
                            completed.insert(job.id());
                        }
                    }
                    for completed_job in completed {
                        let j = running_jobs.remove(&completed_job).unwrap();
                        if let Some(error) = j.take_error() {
                            errors.push(AromaCError::from(error));
                        }
                        if let Err(e) = j.join() {
                            match e.downcast::<CompileError>() {
                                Ok(compile_error) => {
                                    errors.push(AromaCError::from(*compile_error));
                                }
                                Err(e) => {
                                    resume_unwind(e);
                                }
                            }
                        }
                    }
                    for paused_job in to_pause {
                        let j = running_jobs.remove(&paused_job).unwrap();
                        if let Err(e) = j.send(CompileJobCommand::Pause) {
                            errors.push(AromaCError::from(e));
                        } else {
                            paused_jobs.insert(j.id(), j);
                        }
                    }
                }
                errors
            });
            if errors.is_empty() {
                Ok(())
            } else if errors.len() == 1 {
                Err(errors.remove(0))
            } else {
                Err(AromaCError::Errors(errors))
            }
        })
    }

    fn select_next(
        mut paused_jobs: &mut HashMap<CompileJobId, CompileJobHandle>,
    ) -> Option<CompileJobId> {
        paused_jobs
            .iter()
            .filter(|(k, v)| match &*v.status().read() {
                CompileJobStatus::WaitingForIdentifiers(_) => false,
                _ => true,
            })
            .map(|(k, _)| k.clone())
            .next()
    }
}

/// Builder for creating a [AromaC] instance.
#[derive(Debug)]
pub struct AromaCBuilder {
    /// Number of jobs to run at once
    pub jobs: usize,
    pub included: Vec<PathBuf>,
    pub output_directory: PathBuf,
}

impl AromaCBuilder {
    /// Creates an AromaCbuilder with default settings
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the output directory for build object files
    pub fn output_directory<P: AsRef<Path>>(mut self, path: P) -> Self {
        self.output_directory = path.as_ref().to_path_buf();
        self
    }

    /// Builds an [AromaC] instance from this builder
    pub fn build(self) -> Result<AromaC, BuildAromaCError> {
        if self.jobs == 0 {
            return Err(BuildAromaCError::ZeroJobs);
        }

        let output_dir_meta = std::fs::metadata(&self.output_directory).map_err(|e| {
            BuildAromaCError::OutputDirectoryDoesNotExist(self.output_directory.clone(), e)
        })?;
        if !output_dir_meta.is_dir() {
            return Err(BuildAromaCError::OutputDirectoryIsNotADirectory(
                self.output_directory,
            ));
        }
        Ok(AromaC {
            max_jobs: self.jobs,
            output_directory: self.output_directory,
        })
    }
}

impl Default for AromaCBuilder {
    fn default() -> Self {
        Self {
            jobs: num_cpus::get(),
            included: vec![],
            output_directory: PathBuf::from("."),
        }
    }
}

/// An error occurred while building an [AromaC] instance
#[derive(Debug, Error)]
pub enum BuildAromaCError {
    #[error("{0:?} does not exist: {1}")]
    OutputDirectoryDoesNotExist(PathBuf, io::Error),
    #[error("{0:?} is not a directory")]
    OutputDirectoryIsNotADirectory(PathBuf),
    #[error("Compilation can't occur if no jobs are allowed")]
    ZeroJobs,
}
