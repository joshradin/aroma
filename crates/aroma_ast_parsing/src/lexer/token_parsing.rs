use std::str::FromStr;

use aroma_ast::token::TokenKind;
use aroma_common::nom_helpers::identifier_parser;
use nom::branch::alt;
use nom::bytes::complete::{is_not, take_while_m_n, tag};
use nom::character::complete::{alpha1, space0, space1, char, digit1, hex_digit1, multispace1};
use nom::combinator::{all_consuming, consumed, cut, eof, map, map_opt, map_parser, map_res, peek, recognize, value, verify};
use nom::error::{context, ErrorKind, FromExternalError, VerboseError};
use nom::multi::{fold_many0, many0, many1};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, preceded, terminated};
use nom::{IResult, Parser};

type Result<'a, O, E = &'a [u8]> = IResult<&'a [u8], O, VerboseError<E>>;

pub fn parse_token(mut src: &[u8]) -> Result<(usize, TokenKind), String> {
    let mut main_parser = context(
        "token",
        map(delimited(space0, consumed(_parse_token), space0), |(consumed, token)| {
            (consumed.len(), token)
        }),
    );
    (main_parser)(src).map_err(|e| {
        e.map(|e| {
            let VerboseError { errors } = e;
            let errors = errors
                .into_iter()
                .map(|(bytes, kind)| (String::from_utf8_lossy(bytes).as_ref().to_owned(), kind))
                .collect();
            VerboseError { errors }
        })
    })
}

fn _parse_token(src: &[u8]) -> Result<TokenKind> {
    alt((
        parse_eof,
        parse_word,
        parse_literal,
        parse_punctuation,
        parse_operator,
        parse_newline,
    ))(src)
}

fn parse_eof(src: &[u8]) -> Result<TokenKind> {
    context("eof", value(TokenKind::Eof, eof))(src)
}

fn parse_operator(src: &[u8]) -> Result<TokenKind> {
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

fn parse_word(src: &[u8]) -> Result<TokenKind, &[u8]> {
    context(
        "word",
        preceded(
            peek(alpha1),
            cut(map_parser(recognize_identifier, |p| {
                alt((all_consuming(parse_keyword), parse_identifier))(p)
            })),
        ),
    )(src)
}

fn parse_keyword(src: &[u8]) -> Result<TokenKind> {
    context(
        "keyword",
        alt((
            value(TokenKind::If, tag("if")),
            value(TokenKind::Else, tag("else")),
            value(TokenKind::While, tag("while")),
            value(TokenKind::For, tag("for")),
            value(TokenKind::Const, tag("const")),
            value(TokenKind::Let, tag("let")),
            value(TokenKind::Class, tag("class")),
            value(TokenKind::Interface, tag("interface")),
            value(TokenKind::Abstract, tag("abstract")),
            value(TokenKind::Fn, tag("fn")),
            alt((
                value(TokenKind::Public, tag("public")),
                value(TokenKind::Private, tag("private")),
                value(TokenKind::Protected, tag("protected")),
                value(TokenKind::In, tag("in")),
                value(TokenKind::Namespace, tag("namespace")),
                value(TokenKind::Native, tag("native")),
                value(TokenKind::Static, tag("static")),
            )),
        )),
    )(src)
}

fn recognize_identifier(src: &[u8]) -> Result<&[u8]> {
    identifier_parser()(src)
}

fn parse_identifier(src: &[u8]) -> Result<TokenKind> {
    let id_parser = identifier_parser();
    context(
        "identifier",
        map(id_parser, |id: &[u8]| {
            TokenKind::Identifier(
                String::from_utf8(Vec::from(id)).expect("only [0-0a-zA-Z] should be here"),
            )
        }),
    )(src)
}

fn parse_newline(src: &[u8]) -> Result<TokenKind> {
    // doesn't use newline because we don't want incomplete on missing newline
    context("newline", value(TokenKind::Nl, char('\n')))(src)
}

fn parse_punctuation(src: &[u8]) -> Result<TokenKind> {
    context(
        "punctuation",
        alt((
            value(TokenKind::SemiColon, char(';')),
            value(TokenKind::LCurly, char('{')),
            value(TokenKind::RCurly, char('}')),
            value(TokenKind::Comma, char(',')),
        )),
    )(src)
}

fn parse_literal(src: &[u8]) -> Result<TokenKind> {
    context(
        "literal",
        alt((
            map(parse_boolean, |b| TokenKind::Boolean(b)),
            map(parse_hexadecimal_value, |hex| TokenKind::Integer(hex)),
            map(parse_floating_point_value, |float| TokenKind::Float(float)),
            map(parse_integer_value, |hex| TokenKind::Integer(hex)),
            map(parse_string_value, |string| TokenKind::String(string)),
        )),
    )(src)
}

fn parse_hexadecimal_value(input: &[u8]) -> Result<i64> {
    map_res(
        preceded(
            alt((tag("0x"), tag("0X"))),
            cut(recognize(many1(terminated(hex_digit1, many0(char('_')))))),
        ),
        |out: &[u8]| {
            std::str::from_utf8(out)
                .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, e))
                .and_then(|s| {
                    i64::from_str_radix(s, 16)
                        .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, e))
                })
        },
    )(input)
}

fn parse_integer_value(input: &[u8]) -> Result<i64> {
    map_res(
        recognize(many1(terminated(digit1, many0(char('_'))))),
        |out: &[u8]| {
            std::str::from_utf8(out)
                .map_err(|s| VerboseError::from_external_error(input, ErrorKind::Verify, s))
                .and_then(|s| {
                    i64::from_str(s)
                        .map_err(|e| VerboseError::from_external_error(input, ErrorKind::Verify, e))
                })
        },
    )(input)
}

fn parse_floating_point_value(input: &[u8]) -> Result<f64> {
    map_res(recognize_float, |out: &[u8]| {
        std::str::from_utf8(out)
            .map_err(|s| VerboseError::from_external_error(input, ErrorKind::Verify, s))
            .and_then(|s| {
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
            })
    })(input)
}

fn parse_boolean(input: &[u8]) -> Result<bool> {
    alt((value(true, tag("true")), value(false, tag("false"))))(input)
}

#[derive(Debug, Clone)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWs,
}

fn parse_literal_str(input: &[u8]) -> Result<&str> {
    let not_quoted = map_res(is_not("\"\\"), |s: &[u8]| std::str::from_utf8(s));
    verify(not_quoted, |s: &str| !s.is_empty())(input)
}

fn parse_unicode(input: &[u8]) -> Result<std::primitive::char> {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c| char::from(c).is_ascii_hexdigit());

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
    let parse_u32 = map_res(
        map_res(parse_delimited_hex, |s| std::str::from_utf8(s)),
        move |hex| u32::from_str_radix(hex, 16),
    );

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, std::char::from_u32).parse(input)
}
fn parse_escaped_char(input: &[u8]) -> Result<std::primitive::char> {
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

fn parse_escaped_whitespace(input: &[u8]) -> Result<&[u8]> {
    preceded(char('\\'), multispace1)(input)
}

fn parse_string_fragment(input: &[u8]) -> Result<StringFragment> {
    alt((
        map(parse_literal_str, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWs, parse_escaped_whitespace),
    ))(input)
}

fn parse_string_value(input: &[u8]) -> Result<String> {
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

fn parse_insignificant(src: &[u8]) -> Result<&[u8], &[u8]> {
    context(
        "insignificant",
        recognize(alt((
            context("whitespace", space1),
            // recognize(delimited(tag("/*"), take_until("*/"), tag("*/"))),
            // recognize(tuple((tag("//"), many0(not(newline))))),
        ))),
    )(src)
}
