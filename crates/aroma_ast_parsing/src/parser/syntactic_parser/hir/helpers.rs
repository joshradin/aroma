use super::SyntacticParser;
use crate::parser::singletons::{Nl, SemiC};
use crate::parser::{CouldParse, Err, ErrorKind, Parsable, Parser, SyntaxError};
use aroma_ast::token::TokenStream;
use aroma_ast::token::{ToTokens, TokenKind};
use std::io::Read;

#[derive(Debug)]
pub struct Punctuated1<T, P> {
    pub punctuated: Vec<(T, Option<P>)>,
}

impl<T, P> Default for Punctuated1<T, P> {
    fn default() -> Self {
        Self { punctuated: vec![] }
    }
}

impl<'p, T, P> ToTokens<'p> for Punctuated1<T, P>
where
    T: ToTokens<'p>,
    P: ToTokens<'p>,
{
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
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

impl<'p, T, P> Parsable<'p> for Punctuated1<T, P>
where
    T: Parsable<'p>,
    P: Parsable<'p> + CouldParse<'p>,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Err<Self::Err>> {
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

impl<'p, T, P> ToTokens<'p> for Punctuated0<T, P>
where
    T: ToTokens<'p>,
    P: ToTokens<'p>,
{
    fn to_tokens(&self) -> TokenStream<'p, 'p> {
        self.punctuated
            .iter()
            .flat_map(|(item, punc)| {
                item.to_tokens()
                    .chain(punc.iter().flat_map(|i| i.to_tokens()))
            })
            .collect()
    }
}

impl<'p, T, P> Parsable<'p> for Punctuated0<T, P>
where
    T: Parsable<'p> + CouldParse<'p>,
    P: Parsable<'p> + CouldParse<'p>,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Err<Self::Err>> {
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

pub fn cut<'p, P, R, O, E>(parser: P) -> impl Parser<'p, R, O, E>
where
    P: Parser<'p, R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<O, Err<E>> {
        syn_parser.parse(parser.clone()).map_err(|e| match e {
            Err::Error(e) => Err::Failure(e),
            e => e,
        })
    }
}

/// Runs the same parser over and over until failure
pub fn multi0<'p, P, R, O, E>(parser: P) -> impl Parser<'p, R, Vec<O>, E>
where
    P: Parser<'p, R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<Vec<O>, Err<E>> {
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
pub fn multi1<'p, P, R, O, E>(parser: P) -> impl Parser<'p, R, Vec<O>, E>
where
    P: Parser<'p, R, O, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<Vec<O>, Err<E>> {
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
) -> impl Parser<'p, R, Vec<(O1, Option<O2>)>, E>
where
    P: Parser<'p, R, O1, E> + Clone,
    G: Parser<'p, R, O2, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<Vec<(O1, Option<O2>)>, Err<E>> {
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
) -> impl Parser<'p, R, Vec<(O1, Option<O2>)>, E>
where
    P: Parser<'p, R, O1, E> + Clone,
    G: Parser<'p, R, O2, E> + Clone,
    R: Read,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<Vec<(O1, Option<O2>)>, Err<E>> {
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
pub fn map<'p, P, R, O, O2, E, F>(parser: P, map: F) -> impl Parser<'p, R, O2, E>
where
    P: Parser<'p, R, O, E> + Clone,
    R: Read,
    F: FnMut(O) -> O2 + Clone,
    E: std::error::Error,
{
    move |syn_parser: &mut SyntacticParser<'p, R>| -> Result<O2, Err<E>> {
        let parsed = syn_parser.parse(parser.clone())?;
        Ok(map.clone()(parsed))
    }
}

/// End of statement
#[derive(Debug, ToTokens)]
pub enum End<'p> {
    SemiC(SemiC<'p>),
    Nl(Nl<'p>),
}

impl<'p> CouldParse<'p> for End<'p> {
    fn could_parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<bool, Err<Self::Err>> {
        Ok(parser
            .peek()?
            .map(|i| matches!(i.kind(), TokenKind::Nl | TokenKind::SemiColon))
            .unwrap_or(false))
    }
}

impl<'p> Parsable<'p> for End<'p> {
    type Err = SyntaxError<'p>;

    fn parse<R: Read>(
        parser: &mut SyntacticParser<'p, R>,
    ) -> Result<Self, crate::parser::Err<Self::Err>> {
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
