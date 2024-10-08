//! A trait that can provide the [Span] of the complete context of an ir node

use std::convert::Infallible;
use std::io;
use std::io::ErrorKind;
use std::panic::Location;
use std::path::Path;
use std::sync::Arc;
use thiserror::Error;

/// A trait that can provide the [Span] of the complete context of an ir node, fallibly
///
/// This is automatically implemented for all types that implemented [Spanned]
pub trait TrySpanned {
    type Error;

    fn try_span(&self) -> Result<Span, Self::Error>;
}

/// A trait that can provide the [Span] of the complete context of an ir node
///
/// This is automatically implemented for all types that implemented [ToTokens] and
/// [Span] itself.
pub trait Spanned {
    fn span(&self) -> Span;
}

impl<S: Spanned> TrySpanned for S {
    type Error = Infallible;

    fn try_span(&self) -> Result<Span, Self::Error> {
        Ok(self.span())
    }
}

#[derive(Debug, Error)]
#[error("No span available")]
pub struct NoSpanError;

/// The span, representing
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Span {
    path: Arc<Path>,
    offset: usize,
    len: usize,
}

impl Span {
    /// Creates a new span
    pub fn new(path: &Path, start: usize, len: usize) -> Self {
        Self {
            path: Arc::from(path),
            offset: start,
            len,
        }
    }

    /// Creates a new span at the call site of this function
    #[track_caller]
    #[inline]
    pub fn call_site() -> Self {
        let location = Location::caller();
        let loc_line = location.line() as usize;
        let loc_col = location.column() as usize;
        let mut line = 0usize;
        let mut col = 0usize;
        let mut offset = 0usize;
        let path = Path::new(location.file());
        if let Ok(string) = std::fs::read_to_string(path) {
            for char in string.chars() {
                if line + 1 >= loc_line && col + 1 >= loc_col {
                    break;
                }
                if char == '\n' {
                    col = 0;
                    line += 1;
                } else {
                    col += 1;
                }
                offset += char.len_utf8();
            }
        }
        Self::new(path, offset, 0)
    }

    /// Gets a span directly after this span
    pub fn end(&self) -> Self {
        Self {
            path: self.path.clone(),
            offset: self.offset + self.len,
            len: 0,
        }
    }

    /// Tries to get the line and column of a span
    pub fn get_line_col(&self) -> io::Result<(usize, usize)> {
        let mut line = 1usize;
        let mut col = 0usize;
        let string = std::fs::read_to_string(self.file())?;
        for char in string.chars().take(self.offset) {
            if char == '\n' {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
        }
        Ok((line, col))
    }

    /// Gets the full, original line this span is from
    pub fn get_string_line_col(&self) -> io::Result<(String, usize, usize)> {
        let mut line_n = 1usize;
        let mut col = 0usize;
        let mut offset = 0usize;
        let string = std::fs::read_to_string(self.file())?;
        let mut line = vec![];
        for char in string.chars() {
            if char == '\n' {
                if offset < self.offset {
                    col = 0;
                    line_n += 1;
                    line = vec![];
                } else {
                    line.push(char);
                    break;
                }
            } else {
                if offset < self.offset {
                    col += 1;
                }
                line.push(char);
            }
            offset += char.len_utf8();
        }
        Ok((String::from_iter(line), line_n, col))
    }

    /// Gets the file this pan is from
    #[inline]
    pub fn file(&self) -> &Path {
        &*self.path
    }

    /// Creates a span that encompasses both
    pub fn join(&self, other: Self) -> Self {
        let min = self.offset.min(other.offset);
        let max = (self.offset + self.len).max(other.offset + other.len);
        let len = max - min;
        Self {
            path: self.path.clone(),
            offset: min,
            len,
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        self.clone()
    }
}

/// A line reader struct that's responsible for getting lines from a span
#[derive(Debug, Default)]
pub struct LineReader {
    before: usize,
    after: usize,
}

impl LineReader {
    /// Creates a new line reader that gets `before` number of lines before a span and `after` number
    /// of lines after.
    pub fn new(before: usize, after: usize) -> Self {
        Self { before, after }
    }

    /// Gets the lines for a given span, plus the base line index
    pub fn lines(&self, span: &Span) -> io::Result<(Vec<Line>, usize)> {
        let string = std::fs::read_to_string(span.file())?;

        let expected_line_count = 1 + self.before + self.after;
        let mut line_n = 1usize;
        let mut span_start_line = None;
        let mut col = 0usize;
        let mut offset = 0usize;
        let mut lines = vec![];
        let mut current_line = vec![];
        for char in string.chars() {
            if offset == span.offset {
                span_start_line = Some(line_n);
            }
            if char == '\n' {
                if offset < span.offset || lines.len() < expected_line_count {
                    let line = Line {
                        line: line_n,
                        col: if offset >= span.offset { col } else { 0 },
                        byte_offset: offset,
                        src: String::from_iter(current_line),
                    };
                    lines.push(line);
                    col = 0;
                    line_n += 1;
                    current_line = vec![];
                } else {
                    current_line.push(char);
                    break;
                }
            } else {
                if offset < span.offset {
                    col += 1;
                }
                current_line.push(char);
            }
            offset += char.len_utf8();
        }

        if !current_line.is_empty() {
            let line = Line {
                line: line_n,
                col,
                byte_offset: offset,
                src: String::from_iter(current_line),
            };
            lines.push(line);
        }
        let base_line = span_start_line
            .ok_or_else(|| io::Error::new(ErrorKind::UnexpectedEof, "offset out of bounds"))?;
        let range = base_line.saturating_sub(self.before)..=base_line.saturating_add(self.after);
        lines.retain(|line| range.contains(&line.line));

        Ok((lines, base_line))
    }
}

#[derive(Debug)]
pub struct Line {
    pub line: usize,
    pub col: usize,
    pub byte_offset: usize,
    pub src: String,
}

#[cfg(test)]
mod tests {
    use crate::spanned::{Span, Spanned};
    use std::path::{Path, PathBuf};

    #[test]
    fn test_span_lifetime() {
        let path = PathBuf::from("test");
        let p = { Span::new(&path, 0, 0) };
        assert_eq!(&*p.path, path);
        assert_eq!(p.offset, 0);
        assert_eq!(p.len, 0);
    }

    #[test]
    fn test_span_is_spanned() {
        let path = PathBuf::from("test");
        let p = { Span::new(&path, 0, 0).span() };
        assert_eq!(&*p.path, path);
        assert_eq!(p.offset, 0);
        assert_eq!(p.len, 0);
    }

    #[test]
    fn test_span_end() {
        let path = PathBuf::from("test");
        let p = Span::new(&path, 0, 5).end();
        assert_eq!(&*p.path, path);
        assert_eq!(p.offset, 5);
        assert_eq!(p.len, 0);
    }

    #[test]
    fn test_call_site() {
        let p = Span::call_site();
        assert_eq!(p.file(), Path::new(file!()));
    }

    #[test]
    fn test_join() {
        let p1 = Span::new(file!().as_ref(), 0, 8);
        let p2 = Span::new(file!().as_ref(), 16, 8);
        assert_eq!(p1.join(p2), Span::new(file!().as_ref(), 0, 24));
    }
}
