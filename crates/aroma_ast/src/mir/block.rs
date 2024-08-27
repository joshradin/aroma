use crate::mir::statements::Stmt;
use crate::spanned::Span;

/// A block of statements
#[derive(Debug)]
pub struct Block {
    span: Span,
    statements: Vec<Stmt>,
}

impl Block {
    pub fn new(span: Span, statements: impl IntoIterator<Item =Stmt>) -> Self {
        Self {
            span,
            statements: statements.into_iter().collect(),
        }
    }

    /// Creates a new empty block
    pub const fn empty(span: Span) -> Self {
        Self {
            span,
            statements: vec![],
        }
    }
}
