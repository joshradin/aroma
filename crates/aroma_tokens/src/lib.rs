use std::fmt::{Display, Formatter};
use crate::spanned::{LineReader, Span};

pub mod id;
pub mod id_resolver;
pub mod spanned;
pub mod token;

/// An error with a location
#[derive(Debug, thiserror::Error)]
pub struct SpannedError<E, C> {
    error: E,
    location: Option<Span>,
    cause: Option<Box<C>>,
}

impl<E, C> SpannedError<E, C> {
    pub fn new(error: E, location: impl Into<Option<Span>>, cause: impl Into<Option<C>>) -> Self {
        Self { error, location: location.into(), cause: cause.into().map(|i| Box::new(i)) }
    }
}

impl<E: Display, C : Display> Display for SpannedError<E, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.error)?;
        if let Some(location) = &self.location {
            write!(
                f,
                "  -> {}",
                std::fs::canonicalize(location.file())
                    .unwrap_or(location.file().to_path_buf())
                    .to_string_lossy()
            )?;
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
                        writeln!(
                            f,
                            "{}{}{}",
                            " ".repeat(width + 3),
                            " ".repeat(col),
                            "~".repeat(location.len())
                        )?;
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

        Ok(())
    }
}