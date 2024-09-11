//! An aroma compilation error

use itertools::Itertools as _;
use std::io;
use thiserror::Error;
use aroma_ast_parsing::parser::SyntaxError;
use aroma_tokens::id_resolver::{CreateIdError, ResolveIdError};
use aroma_tokens::spanned::Span;
use aroma_tokens::SpannedError;

/// The kind of error
#[derive(Debug, Error)]
pub enum AromaCErrorKind {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("{}", .0.iter().join("\n"))]
    Multi(Vec<AromaCError>),
    #[error(transparent)]
    CreateId(#[from] CreateIdError),
    #[error(transparent)]
    ResolveId(#[from] ResolveIdError),
    #[error(transparent)]
    Syntax(#[from] SyntaxError),
}

/// An error occurred while attempting to compile aroma files
#[derive(Debug, Error)]
#[error(transparent)]
pub struct AromaCError(#[from] SpannedError<AromaCErrorKind, AromaCError>);

impl AromaCError {
    /// Creates a new [AromaCError] using a quick constructor
    #[inline]
    pub fn new(
        error: impl Into<AromaCErrorKind>,
        location: impl Into<Option<Span>>,
        caused_by: impl Into<Option<AromaCError>>,
    ) -> Self {
        Self(SpannedError::new(error.into(), location.into(), caused_by.into()))
    }
}

impl <T> From<T> for AromaCError
    where AromaCErrorKind: From<T>
{
    fn from(value: T) -> Self {
        Self(
            SpannedError::new(
                AromaCErrorKind::from(value),
                None,
                None
            )
        )
    }
}

impl From<Vec<AromaCError>> for AromaCErrorKind {
    fn from(value: Vec<AromaCError>) -> Self {
        AromaCErrorKind::Multi(value)
    }
}

/// A type alias for general results in aromac
pub type AromaCResult<T> = Result<T, AromaCError>;
