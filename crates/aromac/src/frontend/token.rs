//! A lexical token from a source file, along with streams for said token

use crate::common::spanned::{Span, Spanned};

/// A lexical token from a source file
#[derive(Debug, Clone)]
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
}

impl<'p> Spanned<'p> for Token<'p> {
    fn span(&self) -> Span<'p> {
        self.span.clone()
    }
}

/// The kind for this token
#[derive(Debug, Clone)]
pub enum TokenKind {
    SemiColon,

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
    Colon,


    /// =
    Assign,
    /// ==
    Eq,

    /// EOF, will only appear at the end of a token
    Eof
}

/// A stream of tokens
pub struct TokenStream<'a: 'p, 'p>(Box<dyn Iterator<Item = Token<'p>> + 'a>);

impl<'a: 'p, 'p> Iterator for TokenStream<'a, 'p> {
    type Item = Token<'p>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
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
}

impl<'p, T: ToTokens<'p>> Spanned<'p> for T {
    fn span(&self) -> Span<'p> {
        self
            .to_tokens()
            .map::<Span<'p>, _>(|token| token.span())
            .reduce(|a, b| a.join(b).expect("could not join spans"))
            .expect("Spanned has no tokens despite implementing ToTokens")
    }
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
