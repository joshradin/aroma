//! A trait that can provide the [Span] of the complete context of an ir node

use std::cell::{Cell, LazyCell};
use std::io;
use std::panic::Location;
use std::path::{Path, PathBuf};

/// A trait that can provide the [Span] of the complete context of an ir node
///
/// This is automatically implemented for all types that implemented [ToTokens] and
/// [Span] itself.
pub trait Spanned<'p> {
    fn span(&self) -> Span<'p>;
}

/// The span, representing
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Span<'p> {
    path: &'p Path,
    offset: usize,
    len: usize,
    // line_and_col: Cell<(usize, usize)>
}

impl<'p> Span<'p> {
    /// Creates a new span
    pub const fn new(path: &'p Path, start: usize, len: usize) -> Self {
        Self {
            path,
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

    /// leaks this span, creating a permanent memory allocation.
    pub fn leak(self) -> Span<'static> {
        Span {
            path: Box::leak(Box::from(self.path)),
            offset: self.offset,
            len: self.len,
        }
    }

    /// Gets a span directly after this span
    pub const fn end(&self) -> Self {
        Self {
            path: self.path,
            offset: self.offset + self.len,
            len: 0,
        }
    }

    /// Tries to get the line and column of a span
    pub fn get_line_col(&self) -> io::Result<(usize, usize)> {
        let mut line = 1usize;
        let mut col = 0usize;
        let mut offset = 0usize;
        let string = std::fs::read_to_string(self.file())?;
        for char in string.chars().take(self.offset) {
            if char == '\n' {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
            offset += char.len_utf8();
        }
        Ok((line, col))
    }

    /// Gets the file this pan is from
    #[inline]
    pub const fn file(&self) -> &'p Path {
        self.path
    }

    /// Creates a span that encompasses both
    pub fn join(&self, other: Self) -> Option<Self> {
        if self.path != other.path {
            None
        } else {
            let min = self.offset.min(other.offset);
            let max = (self.offset + self.len).max(other.offset + other.len);
            let len = max - min;
            Some(Self {
                path: self.path,
                offset: min,
                len,
            })
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl<'p> Spanned<'p> for Span<'p> {
    fn span(&self) -> Span<'p> {
        *self
    }
}

#[cfg(test)]
mod tests {
    use crate::spanned::{Span, Spanned};
    use std::path::{Path, PathBuf};

    #[test]
    fn test_span_lifetime() {
        let path = PathBuf::from("test");
        let p = { Span::new(&path, 0, 0) };
        assert_eq!(p.path, path);
        assert_eq!(p.offset, 0);
        assert_eq!(p.len, 0);
    }

    #[test]
    fn test_span_is_spanned() {
        let path = PathBuf::from("test");
        let p = { Span::new(&path, 0, 0).span() };
        assert_eq!(p.path, path);
        assert_eq!(p.offset, 0);
        assert_eq!(p.len, 0);
    }

    #[test]
    fn test_span_end() {
        let path = PathBuf::from("test");
        let p = Span::new(&path, 0, 5).end();
        assert_eq!(p.path, path);
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
        assert_eq!(p1.join(p2).unwrap(), Span::new(file!().as_ref(), 0, 24));
    }
}
