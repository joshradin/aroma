use crate::token::{ToTokens, Token, TokenKind, TokenStream};
use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id<'p>(Vec<IdInternal<'p>>);

impl<'p> Id<'p> {
    /// Tries to create an [Id] from an iterator of tokens.
    ///
    /// Returns `Some(Id)` if all tokens given are identifiers
    pub fn new<I: IntoIterator<Item = Token<'p>>>(tokens: I) -> Option<Self> {
        tokens
            .into_iter()
            .try_fold(vec![], |mut accum, next| {
                let internal = IdInternal::new(next)?;
                accum.push(internal);
                Some(accum)
            })
            .map(|vec| Id(vec))
    }

    /// An iterator over the components of an Id
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.0.iter().map(|s| &s.0).map(|tok| {
            if let TokenKind::Identifier(id) = &tok.kind() {
                id.as_ref()
            } else {
                unreachable!()
            }
        })
    }

    /// Gets the most specific member of this id
    pub fn most_specific(&self) -> &str {
        let tok = self.0.last().unwrap();
        if let TokenKind::Identifier(id) = &tok.0.kind() {
            id.as_ref()
        } else {
            unreachable!()
        }
    }

    /// Gets the least specific member of this id
    pub fn least_specific(&self) -> &str {
        let tok = self.0.first().unwrap();
        if let TokenKind::Identifier(id) = &tok.0.kind() {
            id.as_ref()
        } else {
            unreachable!()
        }
    }
}

impl Display for Id<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|t| t.to_string()).join(","))
    }
}

impl<'p> ToTokens<'p> for Id<'p> {
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        TokenStream::from_iter(
            self.0
                .iter()
                .map(|internal| internal.0.clone())
                .collect::<Vec<_>>(),
        )
    }
}

#[derive(Clone)]
struct IdInternal<'p>(Token<'p>);

impl<'p> IdInternal<'p> {
    fn new(token: Token<'p>) -> Option<Self> {
        if let TokenKind::Identifier(_) = token.kind() {
            Some(IdInternal(token))
        } else {
            None
        }
    }
}
impl Debug for IdInternal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}
impl Display for IdInternal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}
impl AsRef<str> for IdInternal<'_> {
    fn as_ref(&self) -> &str {
        let TokenKind::Identifier(s) = self.0.kind() else {
            unreachable!()
        };
        s.as_ref()
    }
}
impl PartialEq for IdInternal<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl Eq for IdInternal<'_> {}
impl Hash for IdInternal<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}
