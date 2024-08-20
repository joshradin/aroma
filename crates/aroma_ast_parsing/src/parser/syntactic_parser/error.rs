use crate::lexer::LexingError;
use aroma_ast::spanned::Span;
use aroma_ast::token::Token;
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::io;

/// Represents an error occurring during parsing
#[derive(Debug, thiserror::Error)]
pub struct SyntaxError<'p> {
    pub location: Option<Span<'p>>,
    pub kind: ErrorKind<'p>,
    pub cause: Option<Box<Self>>,
    pub non_terminal_stack: Option<Vec<&'static str>>,
    line_col: Cell<Option<(usize, usize)>>,
}

impl<'p> SyntaxError<'p> {
    /// Creates a new error
    pub fn new(
        kind: ErrorKind<'p>,
        location: impl Into<Option<Span<'p>>>,
        cause: impl Into<Option<Self>>,
        non_terminals: impl Into<Option<Vec<&'static str>>>,
    ) -> Self {
        Self {
            location: location.into(),
            kind,
            cause: cause.into().map(Box::new),
            non_terminal_stack: non_terminals.into(),
            line_col: Cell::new(None),
        }
    }

    fn get_line_col(&self) -> Option<(usize, usize)> {
        if let Some(location) = &self.location {
            if self.line_col.get().is_none() {
                if let Ok((line, col)) = location.get_line_col() {
                    self.line_col.set(Some((line, col)));
                }
            }
            self.line_col.get()
        } else {
            None
        }
    }
}

impl Display for SyntaxError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "syntax error: {}", self.kind)?;
        if let Some(location) = &self.location {
            write!(f, "  -> {}", location.file().to_string_lossy())?;
            if let Some((line, col)) = self.get_line_col() {
                write!(f, ":{line}:{col}")?;
            }
            writeln!(f)?;
        }
        if let Some(cause) = &self.cause {
            cause.fmt(f)?;
        }
        if let Some(non_terminals) = &self.non_terminal_stack {
            writeln!(f, "non terminal stack:")?;
            for (idx, non_terminal) in non_terminals.iter().enumerate() {
                writeln!(f, "  {}: {}", idx, non_terminal)?;
            }
        }

        Ok(())
    }
}

impl<'p, E> From<E> for SyntaxError<'p>
where
    E: Into<ErrorKind<'p>>,
{
    fn from(value: E) -> Self {
        Self {
            location: None,
            kind: value.into(),
            cause: None,
            non_terminal_stack: None,
            line_col: Cell::new(None),
        }
    }
}

/// [SyntaxError] kind
#[derive(Debug, thiserror::Error)]
pub enum ErrorKind<'p> {
    #[error("illegal statement: {reason}")]
    IllegalStatement { reason: String },
    #[error("Expected a token of kinds {0:?}, got {1:?}")]
    ExpectedToken(Vec<String>, Option<Token<'p>>),
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token<'p>),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("attempting to continue parsing but parser was poisoned")]
    ParserPoisoned,
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Lex(#[from] LexingError),
    #[error("{0}")]
    Custom(&'static str),
}

impl<'p> ErrorKind<'p> {
    pub fn illegal_statement(reason: impl AsRef<str>) -> Self {
        Self::IllegalStatement {
            reason: reason.as_ref().to_string(),
        }
    }

    pub fn expected_token(
        token_kinds: impl IntoIterator<Item = impl AsRef<str>>,
        found: impl Into<Option<Token<'p>>>,
    ) -> Self {
        Self::ExpectedToken(
            token_kinds
                .into_iter()
                .map(|s| s.as_ref().to_string())
                .collect(),
            found.into(),
        )
    }
}

impl From<&'static str> for ErrorKind<'_> {
    fn from(value: &'static str) -> Self {
        todo!()
    }
}

pub type Result<'p, T = ()> = std::result::Result<T, super::Err<SyntaxError<'p>>>;
