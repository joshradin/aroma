//! A lexical token from a source file, along with streams for said token

use crate::spanned::{Span, Spanned};
#[cfg(feature = "derive")]
pub use aroma_ast_derive::ToTokens;
use std::fmt::{Debug, Formatter};
use std::iter;

/// A lexical token from a source file
#[derive(Clone)]
pub struct Token<'p> {
    span: Span<'p>,
    kind: TokenKind,
}

impl<'p> Token<'p> {
    /// Creates a new token
    pub fn new(span: Span<'p>, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    /// Gets the kind for this token
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    /// Leaks this token, making the associated span leak
    pub fn leak(self) -> Token<'static> {
        Token {
            span: self.span.leak(),
            kind: self.kind
        }
    }
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<'p> Spanned<'p> for Token<'p> {
    fn span(&self) -> Span<'p> {
        self.span.clone()
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
}

/// A stream of tokens
pub struct TokenStream<'a: 'p, 'p>(Box<dyn Iterator<Item = Token<'p>> + 'a>);

impl<'p> FromIterator<Token<'p>> for TokenStream<'p, 'p> {
    fn from_iter<T: IntoIterator<Item = Token<'p>>>(iter: T) -> Self {
        Self::from_iter(iter.into_iter().collect::<Vec<_>>())
    }
}

impl<'a: 'p, 'p> Iterator for TokenStream<'a, 'p> {
    type Item = Token<'p>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'p> TokenStream<'p, 'p> {
    #[inline]
    pub fn new() -> Self {
        TokenStream::from_iter(iter::empty())
    }
}
impl<'a: 'p, 'p> TokenStream<'a, 'p> {
    /// Create a token stream from an iterator
    pub fn from_iter<T: IntoIterator<Item = Token<'p>, IntoIter: 'a>>(iter: T) -> Self {
        Self(Box::new(iter.into_iter()))
    }
}

/// A trait to convert something to an iterator of tokens
pub trait ToTokens<'p> {
    /// Gets an iterator over tokens
    fn to_tokens(&self) -> TokenStream<'p, 'p>;

    fn to_token_tree(&self) -> TokenTree<'p> {
        TokenTree::Leaf(Vec::from_iter(self.to_tokens()))
    }
}

impl<'p, T: ToTokens<'p>> Spanned<'p> for T {
    fn span(&self) -> Span<'p> {
        self.to_tokens()
            .map::<Span<'p>, _>(|token| token.span())
            .reduce(|a, b| a.join(b).expect("could not join spans"))
            .expect("Spanned has no tokens despite implementing ToTokens")
    }
}
impl<'p, T: ToTokens<'p>> ToTokens<'p> for Option<T> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        match self {
            None => TokenStream::new(),
            Some(s) => s.to_tokens(),
        }
    }
}
impl<'p, T: ToTokens<'p>> ToTokens<'p> for Vec<T> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        self.iter().flat_map(|t| t.to_tokens()).collect()
    }

    fn to_token_tree(&self) -> TokenTree<'p> {
        TokenTree::Node(self.iter().map(|t| t.to_token_tree()).collect())
    }
}

/// A way of representing tokens in a tree format
#[derive(Debug)]
pub enum TokenTree<'p> {
    Leaf(Vec<Token<'p>>),
    Node(Vec<TokenTree<'p>>),
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
