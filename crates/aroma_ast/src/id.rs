use crate::spanned::{Span, Spanned};
use crate::token::{ToTokens, Token, TokenKind, TokenStream};
use itertools::Itertools;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Id(Vec<IdInternal>);

impl Id {
    /// Tries to create an [Id] from an iterator of tokens.
    ///
    /// Returns `Some(Id)` if all tokens given are identifiers
    pub fn new<I: IntoIterator<Item = Token>>(tokens: I) -> Option<Self> {
        tokens
            .into_iter()
            .try_fold(vec![], |mut accum, next| {
                let internal = IdInternal::new(next)?;
                accum.push(internal);
                Some(accum)
            })
            .map(Id)
    }

    /// Tries to create an [Id] from an iterator of tokens.
    ///
    /// Returns `Some(Id)` if all tokens given are identifiers
    #[track_caller]
    pub fn new_call_site<I: IntoIterator<Item: AsRef<str>>>(tokens: I) -> Option<Self> {
        let r = tokens.into_iter().fold(vec![], |mut accum, next| {
            let internal = IdInternal::new(Token::new(
                Span::call_site(),
                TokenKind::Identifier(next.as_ref().to_string()),
            ))
            .unwrap();
            accum.push(internal);
            accum
        });
        if r.is_empty() {
            None
        } else {
            Some(Id(r))
        }
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

    /// Joins two ids together, changing the span of it's all the children in other to right after this span
    pub fn concat(&self, other: &Self) -> Self {
        let mut inner = self.0.clone();
        let end = self.span().end();
        for id in &other.0 {
            inner.push(IdInternal(Token::new(end.clone(), id.0.kind().clone())));
        }
        Self(inner)
    }

    /// Joins two ids together, changing the span of it's all of this id to be before the other span
    pub fn resolve(&self, other: &Self) -> Self {
        let mut inner = vec![];
        let span = other.span();
        let start = Span::new(span.file(), span.offset(), 0);
        for id in &self.0 {
            inner.push(IdInternal(Token::new(start.clone(), id.0.kind().clone())));
        }
        inner.extend(other.0.clone());
        Self(inner)
    }

    /// Joins two ids together, changing the span of it's all of this id to be before the other span
    #[inline]
    pub fn join<S: AsRef<str> + ?Sized>(&self, other: &S) -> Self {
        self.concat(&Id::new_call_site([other]).unwrap())
    }
}
impl Debug for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Id({:?})",
            self.0.iter().map(|t| t.to_string()).join(".")
        )
    }
}
impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|t| t.to_string()).join("."))
    }
}

impl ToTokens for Id {
    fn to_tokens(&self) -> TokenStream {
        TokenStream::from_iter(
            self.0
                .iter()
                .map(|internal| internal.0.clone())
                .collect::<Vec<_>>(),
        )
    }
}

#[derive(Clone)]
struct IdInternal(Token);

impl IdInternal {
    fn new(token: Token) -> Option<Self> {
        if let TokenKind::Identifier(_) = token.kind() {
            Some(IdInternal(token))
        } else {
            None
        }
    }
}
impl Debug for IdInternal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}
impl Display for IdInternal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}
impl AsRef<str> for IdInternal {
    fn as_ref(&self) -> &str {
        let TokenKind::Identifier(s) = self.0.kind() else {
            unreachable!()
        };
        s.as_ref()
    }
}
impl PartialEq for IdInternal {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}
impl Eq for IdInternal {}
impl Hash for IdInternal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}
