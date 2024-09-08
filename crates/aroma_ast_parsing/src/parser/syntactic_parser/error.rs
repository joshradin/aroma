use crate::lexer::LexingError;
use aroma_tokens::id::Id;
use aroma_tokens::spanned::{LineReader, Span};
use aroma_tokens::token::Token;
use aroma_tokens::SpannedError;
use aroma_types::class::{ClassInst, ClassRef};
use std::fmt::{Display, Formatter};
use std::io;

/// Represents an error occurring during parsing
#[derive(Debug, thiserror::Error)]
pub struct SyntaxError {
    pub kind: SpannedError<ErrorKind, SyntaxError>,
    pub non_terminal_stack: Option<Vec<&'static str>>,
    // line_col: Cell<Option<(usize, usize)>>,
}

impl SyntaxError {
    /// Creates a new error
    pub fn new(
        kind: ErrorKind,
        location: impl Into<Option<Span>>,
        cause: impl Into<Option<Self>>,
        non_terminals: impl Into<Option<Vec<&'static str>>>,
    ) -> Self {
        Self {
            kind: SpannedError::new(kind, location, cause),
            non_terminal_stack: non_terminals.into(),
            // line_col: Cell::new(None),
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "syntax error: {}", self.kind)?;
        if let Some(non_terminals) = &self.non_terminal_stack {
            writeln!(f, "non terminal stack:")?;
            for (idx, non_terminal) in non_terminals.iter().enumerate() {
                writeln!(f, "  {}: {}", idx, non_terminal)?;
            }
        }

        Ok(())
    }
}

impl<'p, E> From<E> for SyntaxError
where
    E: Into<ErrorKind>,
{
    fn from(value: E) -> Self {
        Self::new(value.into(), None, None, None)
    }
}

/// [SyntaxError] kind
#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("illegal statement: {reason}")]
    IllegalStatement { reason: String },
    #[error("Expected a token of kinds {0:?}, got {1:?}")]
    ExpectedToken(Vec<String>, Option<Token>),
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token),
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
    #[error("Constructors can not be static")]
    ConstructorsCanNotBeStatic,
    #[error("Can not declare {0:} as abstract in concrete class {1:?}")]
    AbstractMethodInConcreteClass(String, ClassRef),
    #[error("undeclared variable {0:?}")]
    UndeclaredVariable(Id),
    #[error("Interfaces can not have object fields (only statically declared fields are allowed)")]
    InterfacesCanNotHaveObjectFields,
}

impl ErrorKind {
    pub fn illegal_statement(reason: impl AsRef<str>) -> Self {
        Self::IllegalStatement {
            reason: reason.as_ref().to_string(),
        }
    }

    pub fn expected_token(
        token_kinds: impl IntoIterator<Item = impl AsRef<str>>,
        found: impl Into<Option<Token>>,
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

impl From<&'static str> for ErrorKind {
    fn from(value: &'static str) -> Self {
        todo!()
    }
}

pub type SyntaxResult<T = ()> = std::result::Result<T, super::Err<SyntaxError>>;
