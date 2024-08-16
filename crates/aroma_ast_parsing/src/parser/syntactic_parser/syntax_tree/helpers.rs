use super::SyntacticParser;
use crate::parser::{CouldParse, Parse};
use aroma_ast::token::ToTokens;
use aroma_ast::token::TokenStream;
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

impl<'p, T, P> Parse<'p> for Punctuated1<T, P>
where
    T: Parse<'p>,
    P: Parse<'p> + CouldParse<'p>,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let mut vec = vec![];
        loop {
            let v = T::parse(parser)?;
            if P::could_parse(parser)? {
                let punc = P::parse(parser)?;
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

impl<T, P> Default for Punctuated0<T, P> {
    fn default() -> Self {
        Self { punctuated: vec![] }
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

impl<'p, T, P> Parse<'p> for Punctuated0<T, P>
where
    T: Parse<'p> + CouldParse<'p>,
    P: Parse<'p> + CouldParse<'p>,
    T::Err: From<P::Err>,
{
    type Err = T::Err;

    fn parse<R: Read>(parser: &mut SyntacticParser<'p, R>) -> Result<Self, Self::Err> {
        let mut vec = vec![];
        if T::could_parse(parser)? {
            loop {
                let v = T::parse(parser)?;
                if P::could_parse(parser)? {
                    let punc = P::parse(parser)?;
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
