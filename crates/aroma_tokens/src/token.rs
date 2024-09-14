//! A lexical token from a source file, along with streams for said token

use crate::spanned::{Span, Spanned};
#[cfg(feature = "derive")]
pub use aroma_tokens_derive::ToTokens;
use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::iter;

/// A lexical token from a source file
#[derive(Clone)]
pub struct Token {
    span: Span,
    kind: TokenKind,
}

impl Token {
    /// Creates a new token
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    /// Gets the kind for this token
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Spanned for Token {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

/// The kind for this token
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    If,
    Else,
    While,
    For,
    Class,
    Interface,
    Abstract,

    Fn,
    Const,
    Let,

    Identifier(String),

    // literals
    Float(f64),
    Integer(i64),
    String(String),
    Boolean(bool),
    This,
    Super,
    Delegate,

    /// new line
    Nl,
    SemiColon,

    /// =
    Assign,
    /// ==
    Eq,
    /// :
    Colon,
    Comma,

    /// EOF, will only appear at the end of a token
    Eof,
    LCurly,
    RCurly,
    Plus,
    Minus,
    Div,
    Star,
    LBracket,
    RBracket,
    LParen,
    RParen,
    PlusAssign,
    MinusAssign,
    DivAssign,
    MultAssign,
    Rem,
    RemAssign,
    Neq,
    Bang,
    Public,
    Private,
    In,
    Protected,
    Namespace,
    Dot,
    Arrow,
    Native,
    Static,
    Hash,
    And,
    BitwiseAnd,
    Or,
    BitwiseOr,
    BitwiseXor,
    Lt,
    LShift,
    RShift,
    Lte,
    Gte,
    Gt,
    QMark,
    Out,
    Try,
    Catch,
    Match,
    Return,
    Loop,
    Break,
    Continue,
    Implements,
    Extends,
    True,
    False,
    Null,
    Final,
    Throws,
    Constructor,
    Import,
    DocComment(String),
}

/// A stream of tokens
pub struct TokenStream(VecDeque<Token>);

impl FromIterator<Token> for TokenStream {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop_front()
    }
}

impl Default for TokenStream {
    fn default() -> Self {
        Self::new()
    }
}

impl TokenStream {
    #[inline]
    pub fn new() -> Self {
        TokenStream::from_iter(iter::empty())
    }
}

/// A trait to convert something to an iterator of tokens
pub trait ToTokens {
    /// Gets an iterator over tokens
    fn to_tokens(&self) -> TokenStream;

    fn to_token_tree(&self) -> TokenTree {
        TokenTree::Leaf(Vec::from_iter(self.to_tokens()))
    }
}

impl<T: ToTokens> Spanned for T {
    fn span(&self) -> Span {
        self.to_tokens()
            .map::<Span, _>(|token| token.span())
            .reduce(|a, b| a.join(b))
            .expect("Spanned has no tokens despite implementing ToTokens")
    }
}
impl<T: ToTokens> ToTokens for Option<T> {
    fn to_tokens(&self) -> TokenStream {
        match self {
            None => TokenStream::new(),
            Some(s) => s.to_tokens(),
        }
    }
}
impl<T: ToTokens> ToTokens for Vec<T> {
    fn to_tokens(&self) -> TokenStream {
        self.iter().flat_map(|t| t.to_tokens()).collect()
    }

    fn to_token_tree(&self) -> TokenTree {
        TokenTree::Node(self.iter().map(|t| t.to_token_tree()).collect())
    }
}

/// A way of representing tokens in a tree format
#[derive(Debug)]
pub enum TokenTree {
    Leaf(Vec<Token>),
    Node(Vec<TokenTree>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_token_stream() {
        let vec: Vec<Token> = vec![];
        let mut stream = TokenStream::from_iter(vec);
        assert!(stream.next().is_none())
    }
}
