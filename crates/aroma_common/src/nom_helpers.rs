//! nom helpers

use nom::character::complete::{alpha1, alphanumeric0};
use nom::combinator::recognize;
use nom::error::ParseError;
use nom::sequence::tuple;
use nom::{AsChar, IResult, InputLength, InputTakeAtPosition, Offset, Parser, Slice};
use std::ops::RangeTo;

pub fn identifier_parser<I, E>() -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone + Offset + Slice<RangeTo<usize>> + InputTakeAtPosition<Item: AsChar>,
    E: ParseError<I>,
{
    recognize(tuple((alpha1, alphanumeric0)))
}

#[cfg(test)]
mod tests {
    use crate::nom_helpers::identifier_parser;
    use nom::Finish;

    #[test]
    fn test_one_letter_variable() {
        let id = "i ";
        let (rest, parsed) = identifier_parser::<_, nom::error::Error<_>>()(id).finish().unwrap();
        assert_eq!(parsed, "i");
    }
}
