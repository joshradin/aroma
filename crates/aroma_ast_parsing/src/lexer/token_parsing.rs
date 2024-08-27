use std::str::FromStr;

use aroma_ast::token::TokenKind;
use aroma_common::nom_helpers::recognize_identifier;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_till, take_until, take_while_m_n};
use nom::character::complete::{alpha1, char, digit1, hex_digit1, multispace1, newline, space1};
use nom::character::is_newline;
use nom::combinator::{
    all_consuming, consumed, cut, eof, map, map_opt, map_parser, map_res, peek, recognize, rest,
    value, verify,
};
use nom::error::{context, ErrorKind, FromExternalError, VerboseError};
use nom::multi::{fold_many0, many0, many1};
use nom::number::streaming::recognize_float;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Parser};

type Result<'a, O, E = &'a str> = IResult<&'a str, O, VerboseError<E>>;

pub fn parse_token(src: &str) -> Result<(usize, usize, usize, TokenKind), String> {
    let mut main_parser = context(
        "token",
        map(
            tuple((
                consumed(parse_insignificant),
                consumed(_parse_token),
                consumed(parse_insignificant),
            )),
            |((l, _), (consumed, token), (r, _))| (l.len(), consumed.len(), r.len(), token),
        ),
    );
    (main_parser)(src).map_err(|e| e.map(map_error))
}

fn map_error(e: VerboseError<&str>) -> VerboseError<String> {
    let VerboseError { errors } = e;
    let errors = errors
        .into_iter()
        .map(|(bytes, kind)| (bytes.to_string(), kind))
        .collect();
    VerboseError { errors }
}

fn _parse_token(src: &str) -> Result<TokenKind> {
    alt((
        parse_eof,
        parse_word,
        parse_literal,
        parse_punctuation,
        parse_operator,
        parse_newline,
    ))(src)
}

fn parse_eof(src: &str) -> Result<TokenKind> {
    context("eof", value(TokenKind::Eof, eof))(src)
}

fn parse_operator(src: &str) -> Result<TokenKind> {
    context(
        "operator",
        alt((
            value(TokenKind::Eq, tag("==")),
            value(TokenKind::Neq, tag("!=")),
            value(TokenKind::Bang, char('!')),
            value(TokenKind::Assign, char('=')),
            value(TokenKind::Colon, char(':')),
            value(TokenKind::PlusAssign, tag("+=")),
            value(TokenKind::Plus, char('+')),
            value(TokenKind::Arrow, tag("->")),
            value(TokenKind::MinusAssign, tag("-=")),
            value(TokenKind::Minus, char('-')),
            value(TokenKind::DivAssign, tag("/=")),
            alt((
                value(TokenKind::Div, char('/')),
                value(TokenKind::RemAssign, tag("%=")),
                value(TokenKind::Rem, char('%')),
                value(TokenKind::MultAssign, tag("*=")),
                value(TokenKind::Star, char('*')),
                value(TokenKind::LBracket, char('[')),
                value(TokenKind::RBracket, char(']')),
                value(TokenKind::LParen, char('(')),
                value(TokenKind::RParen, char(')')),
                value(TokenKind::Dot, char('.')),
                value(TokenKind::Hash, char('#')),
                alt((
                    value(TokenKind::And, tag("&&")),
                    value(TokenKind::BitwiseAnd, char('&')),
                    value(TokenKind::Or, tag("||")),
                    value(TokenKind::BitwiseOr, char('|')),
                    value(TokenKind::BitwiseXor, char('^')),
                    value(TokenKind::LShift, tag("<<")),
                    value(TokenKind::RShift, tag(">>")),
                    value(TokenKind::Lte, tag("<=")),
                    value(TokenKind::Lt, char('<')),
                    value(TokenKind::Gte, tag(">=")),
                    value(TokenKind::Gt, char('>')),
                )),
            )),
        )),
    )(src)
}

fn parse_word(src: &str) -> Result<TokenKind> {
    context(
        "word",
        preceded(
            peek(alpha1),
            cut(map_parser(recognize_identifier, |p| {
                alt((parse_keyword, parse_identifier))(p)
            })),
        ),
    )(src)
}
fn all_consuming_tag(src: &str) -> impl FnMut(&str) -> Result<&str> + '_ {
    move |i| all_consuming(tag(src))(i)
}
fn parse_keyword(src: &str) -> Result<TokenKind> {
    context(
        "keyword",
        alt((
            alt((
                value(TokenKind::If, all_consuming_tag("if")),
                value(TokenKind::Else, all_consuming_tag("else")),
                value(TokenKind::While, all_consuming_tag("while")),
                value(TokenKind::For, all_consuming_tag("for")),
                value(TokenKind::Const, all_consuming_tag("const")),
                value(TokenKind::Let, all_consuming_tag("let")),
                value(TokenKind::Class, all_consuming_tag("class")),
                value(TokenKind::Interface, all_consuming_tag("interface")),
                value(TokenKind::Abstract, all_consuming_tag("abstract")),
                value(TokenKind::Fn, all_consuming_tag("fn")),
            )),
            alt((
                value(TokenKind::Public, all_consuming_tag("public")),
                value(TokenKind::Private, all_consuming_tag("private")),
                value(TokenKind::Protected, all_consuming_tag("protected")),
                value(TokenKind::In, all_consuming_tag("in")),
                value(TokenKind::Out, all_consuming_tag("out")),
                value(TokenKind::Namespace, all_consuming_tag("namespace")),
                value(TokenKind::Native, all_consuming_tag("native")),
                value(TokenKind::Static, all_consuming_tag("static")),
                value(TokenKind::Try, all_consuming_tag("try")),
                value(TokenKind::Catch, all_consuming_tag("catch")),
                value(TokenKind::Match, all_consuming_tag("match")),
            )),
            alt((
                value(TokenKind::Return, all_consuming_tag("return")),
                value(TokenKind::Loop, all_consuming_tag("loop")),
                value(TokenKind::Break, all_consuming_tag("break")),
                value(TokenKind::Continue, all_consuming_tag("continue")),
                value(TokenKind::Extends, all_consuming_tag("extends")),
                value(TokenKind::Implements, all_consuming_tag("implements")),
                value(TokenKind::Boolean(true), all_consuming_tag("true")),
                value(TokenKind::Boolean(false), all_consuming_tag("false")),
                value(TokenKind::Null, all_consuming_tag("null")),
                value(TokenKind::Final, all_consuming_tag("final")),
                value(TokenKind::Throws, all_consuming_tag("throws")),
            )),
            alt((
                value(TokenKind::Constructor, all_consuming_tag("constructor")),
                value(TokenKind::Import, all_consuming_tag("import")),
                value(TokenKind::This, all_consuming_tag("this")),
                value(TokenKind::Super, all_consuming_tag("super")),
                value(TokenKind::Delegate, all_consuming_tag("delegate")),
            )),
        )),
    )(src)
}

fn parse_identifier(src: &str) -> Result<TokenKind> {
    let id_parser = recognize_identifier;
    context(
        "identifier",
        map(id_parser, |id: &str| {
            TokenKind::Identifier(
                String::from_utf8(Vec::from(id)).expect("only [0-0a-zA-Z] should be here"),
            )
        }),
    )(src)
}

fn parse_newline(src: &str) -> Result<TokenKind> {
    // doesn't use newline because we don't want incomplete on missing newline
    context("newline", value(TokenKind::Nl, char('\n')))(src)
}

fn parse_punctuation(src: &str) -> Result<TokenKind> {
    context(
        "punctuation",
        alt((
            value(TokenKind::SemiColon, char(';')),
            value(TokenKind::LCurly, char('{')),
            value(TokenKind::RCurly, char('}')),
            value(TokenKind::Comma, char(',')),
            value(TokenKind::QMark, char('?')),
        )),
    )(src)
}

fn parse_literal(src: &str) -> Result<TokenKind> {
    context(
        "literal",
        alt((
            map(parse_boolean, TokenKind::Boolean),
            map(parse_hexadecimal_value, TokenKind::Integer),
            map(parse_floating_point_value, TokenKind::Float),
            map(parse_integer_value, TokenKind::Integer),
            map(parse_string_value, TokenKind::String),
        )),
    )(src)
}

fn parse_hexadecimal_value(input: &str) -> Result<i64> {
    map_res(
        preceded(
            alt((tag("0x"), tag("0X"))),
            cut(recognize(many1(terminated(hex_digit1, many0(char('_')))))),
        ),
        |out: &str| {
            i64::from_str_radix(out, 16)
                .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, e))
        },
    )(input)
}

fn parse_integer_value(input: &str) -> Result<i64> {
    map_res(
        recognize(many1(terminated(digit1, many0(char('_'))))),
        |out: &str| {
            i64::from_str(out)
                .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, e))
        },
    )(input)
}

fn parse_floating_point_value(input: &str) -> Result<f64> {
    map_res(recognize_float, |s: &str| {
        if s.contains(".") || s.contains("e") {
            f64::from_str(s)
                .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, s))
        } else {
            Err(VerboseError::from_external_error(
                input,
                ErrorKind::Verify,
                s,
            ))
        }
    })(input)
}

fn parse_boolean(input: &str) -> Result<bool> {
    alt((value(true, tag("true")), value(false, tag("false"))))(input)
}

#[derive(Debug, Clone)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWs,
}

fn parse_literal_str(input: &str) -> Result<&str> {
    let not_quoted = is_not("\"\\");
    verify(not_quoted, |s: &str| !s.is_empty())(input)
}

fn parse_unicode(input: &str) -> Result<std::primitive::char> {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n::<_, &str, _>(1, 6, |c| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parser. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32).parse(input)
}
fn parse_escaped_char(input: &str) -> Result<std::primitive::char> {
    preceded(
        char('\\'),
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )
    .parse(input)
}

fn parse_escaped_whitespace(input: &str) -> Result<&str> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_string_fragment(input: &str) -> Result<StringFragment> {
    alt((
        map(parse_literal_str, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWs, parse_escaped_whitespace),
    ))(input)
}

fn parse_string_value(input: &str) -> Result<String> {
    let build_string = fold_many0(
        parse_string_fragment,
        String::new,
        |accum, next| match next {
            StringFragment::Literal(l) => {
                format!("{accum}{l}")
            }
            StringFragment::EscapedChar(c) => {
                format!("{accum}{c}")
            }
            StringFragment::EscapedWs => accum,
        },
    );

    delimited(char('"'), cut(build_string), char('"'))(input)
}

fn parse_insignificant(src: &str) -> Result<()> {
    context(
        "insignificant",
        map(
            alt((
                recognize(eof),
                recognize(many0(alt((
                    context("whitespace", space1),
                    context(
                        "block comment",
                        recognize(delimited(tag("/*"), take_until("*/"), tag("*/"))),
                    ),
                    context(
                        "line comment",
                        recognize(delimited(
                            tag("//"),
                            take_till(|s| is_newline(s as u8)),
                            newline,
                        )),
                    ),
                    context("line comment", recognize(preceded(tag("//"), rest))),
                )))),
            )),
            |_| (),
        ),
    )(src)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Finish;
    use test_log::test;

    #[test]
    fn test_insignificant() {
        let (rest, ()) = all_consuming(parse_insignificant)("/* hello */")
            .finish()
            .unwrap_or_else(|e| panic!("{}", map_error(e)));
        assert_eq!(rest, "", "should be empty but got {rest:?}");
        let (rest, ()) = all_consuming(parse_insignificant)("// whats her name?\n")
            .finish()
            .unwrap_or_else(|e| panic!("{}", map_error(e)));
        assert_eq!(rest, "", "should be empty but got {rest:?}");
        let (rest, ()) = all_consuming(parse_insignificant)("// whats his name?\n// blah")
            .finish()
            .unwrap_or_else(|e| panic!("{}", map_error(e)));
        assert_eq!(rest, "", "should be empty but got {rest:?}");
    }

    #[test]
    fn test_parse_keyword() {
        let (rest, TokenKind::Class) =
            all_consuming(parse_word)("class").expect("could not parser")
        else {
            panic!()
        };
        let (rest, TokenKind::Constructor) =
            all_consuming(parse_word)("constructor").expect("could not parser")
        else {
            panic!()
        };
    }
}
