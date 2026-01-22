use std::ops::Range;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take, take_while1},
    character::complete::{multispace1, none_of},
    combinator::{map, opt, recognize, value},
    error::{context, VerboseError},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};

use nom_locate::LocatedSpan;
use url::Url;

use crate::types::{CurlFlag, CurlFlagType};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
struct CurlmanToken<'a> {
    pub token_type: CurlmanTokenType,
    pub lexeme: &'a str,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq)]
enum CurlmanIr {
    Flag {
        flag_token_idx: usize,
        value_token_idx: Option<usize>,
        value: CurlFlag, // pub struct CurlFlag { flag_type: CurlFlagType, value: Option<String>, }
    },
    Url {
        token_idx: usize,
        value: Url,
    },
}

impl<'a> CurlmanToken<'a> {
    pub fn from_span(input: Span<'a>, token_type: CurlmanTokenType) -> Self {
        let s: &str = *input.fragment();
        let start = input.location_offset();
        let end = start + s.len();

        Self {
            token_type,
            lexeme: s,
            span: start..end,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CurlmanTokenType {
    Flag,
    ShortFlag,
    Word,
    String,
}

pub fn parse_short_flag(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    context(
        "Invalid flag",
        map(
            recognize(pair(
                tag("-"),
                take_while1(|ch: char| !ch.is_whitespace() && ch != '-'),
            )),
            |m: Span| CurlmanToken::from_span(m, CurlmanTokenType::ShortFlag),
        ),
    )(input)
}

pub fn parse_long_flag(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    context(
        "Invalid flag",
        map(
            recognize(pair(
                tag("--"),
                take_while1(|ch: char| ch.is_alphanumeric() || ch == '-' || ch == '.'),
            )),
            |flag: Span| CurlmanToken::from_span(flag, CurlmanTokenType::Flag),
        ),
    )(input)
}

pub fn parse_word(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    map(take_while1(|ch: char| !ch.is_whitespace()), |tag: Span| {
        CurlmanToken::from_span(tag, CurlmanTokenType::Word)
    })(input)
}

pub fn parse_string(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    let double_quoted = tuple((
        tag::<_, _, _>("\""),
        escaped(none_of("\\\""), '\\', take(1usize)),
        tag("\""),
    ));
    let single_quoted = tuple((
        tag::<_, _, _>("\'"),
        escaped(none_of("\\\'"), '\\', take(1usize)),
        tag("\'"),
    ));

    let string_parser = recognize(alt((double_quoted, single_quoted)));

    map(string_parser, |tag: Span| {
        CurlmanToken::from_span(tag, CurlmanTokenType::String)
    })(input)
}

pub fn parse_value(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    alt((parse_string, parse_word))(input)
}

pub fn parse_token(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    alt((parse_short_flag, parse_long_flag, parse_value))(input)
}

fn bash_ws0(input: Span) -> IResult<Span, (), VerboseError<Span>> {
    let line_cont = value(
        (),
        tuple((
            tag::<_, _, _>("\\"),
            opt(tag::<_, _, _>("\r")),
            tag::<_, _, _>("\n"),
        )),
    );

    value((), many0(alt((value((), multispace1), line_cont))))(input)
}

pub fn lex_curlman_request(input: Span) -> IResult<Span, Vec<CurlmanToken>, VerboseError<Span>> {
    let (input, _) = bash_ws0(input)?;
    let (input, _) = tag("curl")(input)?;
    let (input, tokens) = preceded(bash_ws0, many0(terminated(parse_token, bash_ws0)))(input)?;

    Ok((input, tokens))
}

pub fn parse_tokens_into_ir(
    tokens: &[CurlmanToken],
) -> Result<Vec<CurlmanIr>, crate::error::parser::Error> {
    let mut out = Vec::with_capacity(10);

    enum State {
        LookingForFlagValue,
        LookingForUrl,
    }

    let mut state = State::LookingForUrl;
    use CurlmanTokenType::*;

    for (idx, token) in tokens.iter().enumerate() {
        let CurlmanToken {
            token_type, lexeme, ..
        } = token;

        match state {
            State::LookingForFlagValue if *token_type == Flag || *token_type == ShortFlag => {
                return Err(crate::error::parser::Error::InvalidFlagValue(format!(
                    "Expected flag value and got token: {token:?}",
                )))
            }
            State::LookingForFlagValue => {
                let value = Some(lexeme.to_string());

                match out.last_mut() {
                    Some(CurlmanIr::Flag {
                        value_token_idx,
                        value: flag,
                        ..
                    }) => {
                        *value_token_idx = Some(idx);
                        flag.value = value;
                    }
                    _ => {
                        return Err(crate::error::parser::Error::InvalidFlagValue(format!(
                            "Expected flag value and got token: {token:?}",
                        )))
                    }
                }

                state = State::LookingForUrl;
            }
            State::LookingForUrl if *token_type == Word || *token_type == String => {
                let url = Url::parse(lexeme).map_err(|e| {
                    crate::error::parser::Error::InvalidUrl(format!("Invalid url: {e:?}"))
                })?;

                out.push(CurlmanIr::Url {
                    token_idx: idx,
                    value: url,
                })
            }
            State::LookingForUrl => {
                if let Ok(flag_type) = lexeme.parse::<CurlFlagType>() {
                    match flag_type.needs_value() {
                        true => {
                            state = State::LookingForFlagValue;
                        }
                        false => {
                            state = State::LookingForUrl;
                        }
                    };

                    out.push(CurlmanIr::Flag {
                        flag_token_idx: idx,
                        value_token_idx: None,
                        value: CurlFlag::new(flag_type, None),
                    });
                }
            }
        }
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::new_parser::{lex_curlman_request, parse_tokens_into_ir, CurlmanToken};
        use crate::new_parser::CurlmanTokenType::*;

    #[test]
    fn test_lexer() {

        let inputs = vec![
            r#"curl --request GET \
  --url 'https://api.x.com/2/tweets/search/recent?query=from%3Atwitterdev&tweet.fields=public_metrics' \
  --header 'Authorization: Bearer $BEARER_TOKEN'"#
        ]
            .into_iter()
            .map(|i| lex_curlman_request(i.into()).map(|res| res.1))
            .collect::<Vec<_>>();

        let expected = vec![
Ok(vec![CurlmanToken { token_type: Flag, lexeme: "--request", span: 5..14 }, CurlmanToken { token_type: Word, lexeme: "GET", span: 15..18 }, CurlmanToken { token_type: Flag, lexeme: "--url", span: 23..28 }, CurlmanToken { token_type: String, lexeme: "'https://api.x.com/2/tweets/search/recent?query=from%3Atwitterdev&tweet.fields=public_metrics'", span: 29..123 }, CurlmanToken { token_type: Flag, lexeme: "--header", span: 128..136 }, CurlmanToken { token_type: String, lexeme: "'Authorization: Bearer $BEARER_TOKEN'", span: 137..174 }])
        ];

        assert_eq!(inputs, expected)
    }

    #[test]
    fn test_ir() {
        let input = 
vec![CurlmanToken { token_type: Flag, lexeme: "--request", span: 5..14 }, CurlmanToken { token_type: Word, lexeme: "GET", span: 15..18 }, CurlmanToken { token_type: Flag, lexeme: "--url", span: 23..28 }, CurlmanToken { token_type: String, lexeme: "'https://api.x.com/2/tweets/search/recent?query=from%3Atwitterdev&tweet.fields=public_metrics'", span: 29..123 }, CurlmanToken { token_type: Flag, lexeme: "--header", span: 128..136 }, CurlmanToken { token_type: String, lexeme: "'Authorization: Bearer $BEARER_TOKEN'", span: 137..174 }];

        let result = parse_tokens_into_ir(&input);

        assert_eq!(Ok(vec![]), result)
    }
}
