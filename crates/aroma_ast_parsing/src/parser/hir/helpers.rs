use crate::parser::blocking::SyntacticParser;
use crate::parser::hir::singletons::{Nl, SemiC};
use crate::parser::{Err, ErrorKind, SyntaxError, SyntaxResult};
use aroma_tokens::token::TokenStream;
use aroma_tokens::token::{ToTokens, TokenKind};
use std::io::Read;
use crate::parser::traits::{CouldParse, Parsable, Parser};

pub trait Punctuated<T> {
    /// Gets the items in this
    fn items(&self) -> Vec<&T>;

    /// Converts this into just its items, losing all punctuation
    fn into_items(self) -> Vec<T>
    where
        Self: Sized;
}

#[derive(Debug)]
pub struct Punctuated1<T, P> {
    pub punctuated: Vec<(T, Option<P>)>,
}

impl<T, P> Punctuated<T> for Punctuated1<T, P> {
    /// Gets the items in this
    fn items(&self) -> Vec<&T> {
        self.punctuated.iter().map(|i| &i.0).collect()
    }

    fn into_items(self) -> Vec<T>
    where
        Self: Sized,
    {
        self.punctuated.into_iter().map(|i| i.0).collect()
    }
}

impl<T, P> Default for Punctuated1<T, P> {
    fn default() -> Self {
        Self { punctuated: vec![] }
    }
}

impl<'p, T, P> ToTokens for Punctuated1<T, P>
where
    T: ToTokens,
    P: ToTokens,
{
    fn to_tokens(&self) -> TokenStream {
        self.punctuated
            .iter()
            .flat_map(|(item, punc)| {
                item.to_tokens()
                    .chain(punc.iter().flat_map(|i| i.to_tokens()))
            })
            .collect()
    }
}

impl<T, P> TryFrom<Vec<(T, Option<P>)>> for Punctuated1<T, P> {
    type Error = &'static str;

    fn try_from(value: Vec<(T, Option<P>)>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Err("can not be empty")
        } else {
            Ok(Punctuated1 { punctuated: value })
        }
    }
}

impl<'p, T, P> Parsable for Punctuated1<T, P>
where
    T: Parsable,
    P: Parsable + CouldParse,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<T::Err>> {
        let mut vec = vec![];
        loop {
            let v = T::parse(parser)?;
            if P::could_parse(parser).map_err(|e| e.convert())? {
                let punc = P::parse(parser).map_err(|e| e.convert())?;
                vec.push((v, Some(punc)));
            } else {
                vec.push((v, None));
                break;
            }
        }
        Ok(Punctuated1 { punctuated: vec })
    }
}

#[derive(Debug)]
pub struct Punctuated0<T, P> {
    pub punctuated: Vec<(T, Option<P>)>,
}

impl<T, P> Punctuated0<T, P> {
    /// Gets the items in this
    pub fn items(&self) -> Vec<&T> {
        self.punctuated.iter().map(|i| &i.0).collect()
    }
}

impl<T, P> Punctuated<T> for Punctuated0<T, P> {
    /// Gets the items in this
    fn items(&self) -> Vec<&T> {
        self.punctuated.iter().map(|i| &i.0).collect()
    }

    fn into_items(self) -> Vec<T>
    where
        Self: Sized,
    {
        self.punctuated.into_iter().map(|i| i.0).collect()
    }
}

impl<T, P> From<Punctuated1<T, P>> for Punctuated0<T, P> {
    fn from(value: Punctuated1<T, P>) -> Self {
        Self {
            punctuated: value.punctuated,
        }
    }
}

impl<T, P> Default for Punctuated0<T, P> {
    fn default() -> Self {
        Self { punctuated: vec![] }
    }
}

impl<T, P> From<Vec<(T, Option<P>)>> for Punctuated0<T, P> {
    fn from(value: Vec<(T, Option<P>)>) -> Self {
        Self { punctuated: value }
    }
}

impl<'p, T, P> ToTokens for Punctuated0<T, P>
where
    T: ToTokens,
    P: ToTokens,
{
    fn to_tokens(&self) -> TokenStream {
        self.punctuated
            .iter()
            .flat_map(|(item, punc)| {
                item.to_tokens()
                    .chain(punc.iter().flat_map(|i| i.to_tokens()))
            })
            .collect()
    }
}

impl<'p, T, P> Parsable for Punctuated0<T, P>
where
    T: Parsable + CouldParse,
    P: Parsable + CouldParse,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> Result<Self, Err<T::Err>> {
        let mut vec = vec![];
        if T::could_parse(parser).map_err(|e| e.convert())? {
            loop {
                let v = T::parse(parser).map_err(|e| e.convert())?;
                if P::could_parse(parser).map_err(|e| e.convert())? {
                    let punc = P::parse(parser).map_err(|e| e.convert())?;
                    vec.push((v, Some(punc)));
                } else {
                    vec.push((v, None));
                    break;
                }
            }
        }
        Ok(Punctuated0 { punctuated: vec })
    }
}

pub fn cut<'p, P, R, O, E>(parser: P) -> impl Parser<R, O, E>
where
    P: Parser<R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<R>| -> Result<O, Err<E>> {
        syn_parser.parse(parser.clone()).map_err(|e| match e {
            Err::Error(e) => Err::Failure(e),
            e => e,
        })
    }
}

/// Runs the same parser over and over until failure
pub fn multi0<'p, P, R, O, E>(parser: P) -> impl Parser<R, Vec<O>, E>
where
    P: Parser<R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<R>| -> Result<Vec<O>, Err<E>> {
        let mut r = vec![];
        while let Some(parsed) = syn_parser
            .try_parse(parser.clone())
            .map_err(|e| Err::Failure(e))?
        {
            r.push(parsed);
        }
        Ok(r)
    }
}

/// Runs the same parser over and over until failure, requiring at least one successful parse
pub fn multi1<'p, P, R, O, E>(parser: P) -> impl Parser<R, Vec<O>, E>
where
    P: Parser<R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<R>| -> Result<Vec<O>, Err<E>> {
        let mut r = vec![];
        let item = syn_parser.parse(parser.clone())?;
        r.push(item);
        while let Some(parsed) = syn_parser
            .try_parse(parser.clone())
            .map_err(|e| Err::Failure(e))?
        {
            r.push(parsed);
        }
        Ok(r)
    }
}

/// Runs the same parser over and over with the given delimiter until failure, requiring at least one successful parse
pub fn seperated_list0<'p, P, G, R, O1, O2, E>(
    sep: G,
    e: P,
) -> impl Parser<R, Vec<(O1, Option<O2>)>, E>
where
    P: Parser<R, O1, E> + Clone,
    G: Parser<R, O2, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<R>| -> Result<Vec<(O1, Option<O2>)>, Err<E>> {
        let mut r = vec![];
        let Some(item) = syn_parser
            .try_parse(e.clone())
            .map_err(|e| Err::Failure(e))?
        else {
            return Ok(vec![]);
        };
        r.push((item, None));
        while let Some(sep) = syn_parser
            .try_parse(sep.clone())
            .map_err(|e| Err::Failure(e))?
        {
            let (last, None) = r.pop().unwrap() else {
                panic!("should always be none initially")
            };
            r.push((last, Some(sep)));
            let item = syn_parser.parse(e.clone())?;
            r.push((item, None));
        }
        Ok(r)
    }
}
/// Runs the same parser over and over with the given delimiter until failure, requiring at least one successful parse
pub fn seperated_list1<'p, P, G, R, O1, O2, E>(
    sep: G,
    e: P,
) -> impl Parser<R, Vec<(O1, Option<O2>)>, E>
where
    P: Parser<R, O1, E> + Clone,
    G: Parser<R, O2, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'_, R>| -> Result<Vec<(O1, Option<O2>)>, Err<E>> {
        let mut r = vec![];
        let item = syn_parser.parse(e.clone())?;
        r.push((item, None));
        while let Some(sep) = syn_parser
            .try_parse(sep.clone())
            .map_err(|e| Err::Failure(e))?
        {
            let (last, None) = r.pop().unwrap() else {
                panic!("should always be none initially")
            };
            r.push((last, Some(sep)));
            let item = syn_parser.parse(e.clone())?;
            r.push((item, None));
        }
        Ok(r)
    }
}

/// Runs the same parser over and over until failure
pub fn map<'p, P, R, O, O2, E, F>(parser: P, map: F) -> impl Parser<R, O2, E>
where
    P: Parser<R, O, E> + Clone,
    R: Read,
    F: FnMut(O) -> O2 + Clone,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<R>| -> Result<O2, Err<E>> {
        let parsed = syn_parser.parse(parser.clone())?;
        Ok(map.clone()(parsed))
    }
}

/// End of statement
#[derive(Debug, ToTokens)]
pub enum End {
    SemiC(SemiC),
    Nl(Nl),
}

impl CouldParse for End {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<R>) -> Result<bool, Err<Self::Err>> {
        Ok(parser
            .peek()?
            .map(|i| matches!(i.kind(), TokenKind::Nl | TokenKind::SemiColon))
            .unwrap_or(false))
    }
}

impl Parsable for End {
    type Err = SyntaxError;

    fn parse<R: Read>(parser: &mut SyntacticParser<'_, R>) -> SyntaxResult<Self> {
        let p = parser
            .consume()?
            .ok_or_else(|| parser.error(ErrorKind::UnexpectedEof, None))?;
        match p.kind() {
            TokenKind::SemiColon => {
                let c = SemiC::try_from(p)?;
                Ok(End::SemiC(c))
            }
            TokenKind::Nl => {
                let c = Nl::try_from(p)?;
                Ok(End::Nl(c))
            }
            _ => Err(parser.error(ErrorKind::expected_token([";", "\\n"], p), None)),
        }
    }
}
