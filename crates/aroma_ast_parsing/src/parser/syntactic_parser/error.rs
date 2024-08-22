use crate::lexer::LexingError;
use aroma_ast::spanned::{LineReader, Span};
use aroma_ast::token::Token;
use std::fmt::{Display, Formatter};
use std::io;

/// Represents an error occurring during parsing
#[derive(Debug, thiserror::Error)]
pub struct SyntaxError<'p> {
    pub location: Option<Span<'p>>,
    pub kind: ErrorKind<'p>,
    pub cause: Option<Box<Self>>,
    pub non_terminal_stack: Option<Vec<&'static str>>,
    // line_col: Cell<Option<(usize, usize)>>,
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
            // line_col: Cell::new(None),
        }
    }
}

impl Display for SyntaxError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "syntax error: {}", self.kind)?;
        if let Some(location) = &self.location {
            write!(f, "  -> {}", std::fs::canonicalize(location.file()).unwrap_or(location.file().to_path_buf()).to_string_lossy())?;
            let (lines, base_line) = LineReader::new(2, 2)
                .lines(location)
                .map_err(|_| std::fmt::Error)?;
            let col = lines
                .iter()
                .find(|line| line.line == base_line)
                .expect("base line should always be present")
                .col;
            writeln!(f, ":{base_line}:{col}")?;
            let width = lines.iter().map(|line| line.line).max().unwrap_or(0) / 10 + 1;
            for line in &lines {
                writeln!(f, "{:width$} | {}", line.line, line.src.trim_end())?;
                if line.line == base_line {
                    let col = line.col;
                    if location.len() > 0 {
                        writeln!(f, "{}{}{}", " ".repeat(width + 3), " ".repeat(col), "~".repeat(location.len()))?;
                    } else {
                        writeln!(f, "{}{}^", " ".repeat(width + 3), "-".repeat(col))?;
                    }
                }
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
            // line_col: Cell::new(None),
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
