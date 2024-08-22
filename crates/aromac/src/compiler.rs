//! Responsible with compiling aroma files into aroma object files

use crate::compiler::compile_job::{CompileError, CompileJob, CompileJobHandle, CompileJobId, CompileJobStatus};
use itertools::Itertools;
use log::{debug, error, info, trace};
use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::panic::{panic_any, resume_unwind};
use std::path::{Path, PathBuf};
use std::{io, thread};
use std::sync::Arc;
use parking_lot::RwLock;
use thiserror::Error;
mod compile_job;

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
    pub fn compile<'p>(&mut self, path: &'p Path) -> Result<(), AromaCError<'p>> {
        self.compile_all([path])
    }

    /// Compile a file at a given path
    pub fn compile_all<'p, I>(&mut self, paths: I) -> Result<(), AromaCError<'p>>
    where
        I: IntoIterator<Item= &'p Path>,
    {
        let paths = paths.into_iter().collect::<HashSet<&'p Path>>();
        let mut errors = thread::scope::<'p, _, _>(|scope| {
            let mut errors: Vec<AromaCError<'p>> = Default::default();
            let mut jobs = HashMap::new();
            for path in paths {
                let job = CompileJob::start(scope, &*path);
                jobs.insert(job.id(), job);
            }
            while !jobs.is_empty() {
                let mut completed = HashSet::<CompileJobId>::new();
                for job in jobs.values() {
                    let status = job.status().read();
                    match *status {
                        CompileJobStatus::Failed(ref f) => {
                            error!("job {:?} failed: {f}", job.id());
                            completed.insert(job.id());
                        }
                        CompileJobStatus::Dead | CompileJobStatus::Done => {
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
                    let j = jobs.remove(&completed_job).unwrap();
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
    }
}

#[derive(Debug, Error)]
pub enum AromaCError<'p> {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    CompileError(CompileError<'p>),
    #[error("{}", .0.iter().join("\n"))]
    Errors(Vec<AromaCError<'p>>)
}

impl<'p> From<CompileError<'p>> for AromaCError<'p> {
    fn from(value: CompileError<'p>) -> Self {
        Self::CompileError(value)
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
    pub fn build(mut self) -> Result<AromaC, BuildAromaCError> {
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
