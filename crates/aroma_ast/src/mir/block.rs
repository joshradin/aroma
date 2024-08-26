use crate::spanned::Span;

/// A block of statements
#[derive(Debug)]
pub struct Block {
    span: Span,
    statements: Vec<()>
}