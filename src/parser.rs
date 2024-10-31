use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take, take_till, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, none_of},
    combinator::recognize,
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

//TODO?
//CONSIDER CURL REDIRECT FLAG?
//consider basic auth?
//file uploads?

use ratatui::style::Color;
use url::{self, Url};

use crate::{
    editor::colors::Colorscheme,
    types::{BodyType, CurlmanRequestParamType, RequestInfo},
};

#[derive(Debug)]
pub enum Token<'a> {
    Curl(&'a str, Color),
    Url(&'a str, Color),
    ParamKey(&'a str, Color),
    ParamValue(&'a str, Color),
    Whitespace(&'a str),
    Separator(&'a str, Color),
    Unknown(&'a str, Color),
}

impl<'a> Token<'a> {
    pub fn get_str(&self) -> &str {
        match self {
            Token::Curl(text, ..) => text,
            Token::Url(text, ..) => text,
            Token::ParamKey(text, ..) => text,
            Token::ParamValue(text, ..) => text,
            Token::Whitespace(text, ..) => text,
            Token::Separator(text, ..) => text,
            Token::Unknown(text, ..) => text,
        }
    }

    pub fn get_color(&self) -> Option<Color> {
        match self {
            Token::Curl(.., color) => Some(*color),
            Token::Url(.., color) => Some(*color),
            Token::ParamKey(.., color) => Some(*color),
            Token::ParamValue(.., color) => Some(*color),
            Token::Separator(.., color) => Some(*color),
            Token::Unknown(.., color) => Some(*color),
            Token::Whitespace(_) => None,
        }
    }
}

fn parse_curl_params<'a>(input: &'a str) -> IResult<&'a str, RequestInfo> {
    let string_parser = alt((
        delimited(char('"'), take_till(|ch: char| ch == '"'), char('"')),
        delimited(char('\''), take_till(|ch: char| ch == '\''), char('\'')),
    ));

    let (input, _) = multispace0(input)?;
    let tag_parser = take_while(|ch: char| ch.is_ascii_alphanumeric());

    let param_parser = recognize(pair(
        take_while(|ch: char| ch == '-'),
        take_while(|ch: char| ch.is_ascii_alphanumeric()),
    ));

    let (input, params) = separated_list0(
        multispace1,
        separated_pair(param_parser, multispace1, alt((string_parser, tag_parser))),
    )(input)?;

    let mut request_info = RequestInfo::default();
    for (param_type, value) in params {
        let param_type_res: Result<CurlmanRequestParamType, _> = param_type.parse();

        let Ok(param_type) = param_type_res else {
            return Err(nom::Err::Failure(Error {
                input,
                code: ErrorKind::IsNot,
            }));
        };

        match param_type {
            CurlmanRequestParamType::Method => {
                let method_res: Result<Method, _> = value.parse();

                let Ok(method) = method_res else {
                    return Err(nom::Err::Failure(Error {
                        input,
                        code: ErrorKind::IsNot,
                    }));
                };

                request_info.method = method
            }
            CurlmanRequestParamType::Header => {
                let (_, (key, value)) =
                    separated_pair(take_till(|ch| ch == ':'), tag(":"), take_while(|_| true))(
                        value,
                    )?;

                request_info
                    .headers
                    .insert(key.to_string(), value.to_string());
            }
            CurlmanRequestParamType::Body(BodyType::Json) => {
                request_info.body = Some(value.as_bytes().into());
            }
        }
    }

    Ok((input, request_info))
}

pub fn parse_curlman_request(input: &str) -> IResult<&str, RequestInfo> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("curl")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, url_str) = take_till(char::is_whitespace)(input)?;

    let url_res: Result<Url, _> = url_str.parse();
    let Ok(url) = url_res else {
        return Err(nom::Err::Failure(Error {
            input,
            code: ErrorKind::IsNot,
        }));
    };
    let (input, _) = multispace1(input)?;
    let (input, mut request_builder) = parse_curl_params(input)?;
    let (input, _) = multispace0(input)?;
    request_builder.url = Some(url);
    Ok((input, request_builder))
}

struct Request {
    info: RequestInfo,
    name: String,
}

pub fn parse_curlman_request_file(input: &str) -> IResult<&str, Vec<RequestInfo>> {
    let (input, requests) = separated_list0(tag("==="), parse_curlman_request)(input)?;

    Ok((input, requests))
}

#[derive(Debug)]
enum ParserState {
    ExpectingCurl,
    ExpectingUrl,
    ExpectingParamKey,
    ExpectingParamValue,
}

fn parse_curlman_editor_line<'a, 'b>(
    input: &'a str,
    parser_state: &'b mut ParserState,
    colorscheme: &Colorscheme,
) -> IResult<&'a str, Vec<Token<'a>>> {
    let mut line_tokens = Vec::new();

    match parser_state {
        ParserState::ExpectingCurl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let (input, curl_tag) = tag("curl")(input)?;
            line_tokens.push(Token::Curl(curl_tag, colorscheme.curl_color));

            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            *parser_state = ParserState::ExpectingUrl;
            return Ok((input, line_tokens));
        }

        ParserState::ExpectingUrl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let (input, url_str) = take_till(char::is_whitespace)(input)?;
            let url_parse_res: Result<Url, _> = url_str.parse();
            line_tokens.push(if url_parse_res.is_ok() {
                Token::Url(url_str, colorscheme.url_color)
            } else {
                Token::Unknown(url_str, colorscheme.unknown_color)
            });

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }
            *parser_state = ParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
        ParserState::ExpectingParamKey => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let separator_res = tag::<_, &str, nom::error::Error<&str>>("===")(input);

            match separator_res {
                Ok((input, separator)) => {
                    *parser_state = ParserState::ExpectingCurl;
                    line_tokens.push(Token::Separator(separator, colorscheme.separator_color));
                    return Ok((input, line_tokens));
                }
                _ => {}
            };

            let mut param_parser = recognize(pair(
                take_while(|ch: char| ch == '-'),
                take_while(|ch: char| ch.is_ascii_alphanumeric()),
            ));

            let (input, param) = param_parser(input)?;

            line_tokens.push(Token::ParamKey(param, colorscheme.param_key_color));

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            *parser_state = ParserState::ExpectingParamValue;
            return Ok((input, line_tokens));
        }
        ParserState::ExpectingParamValue => {
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let tag_parser = take_while1(|ch: char| ch.is_ascii_alphanumeric());

            let string_parser = recognize(tuple((
                tag("\""),
                escaped(none_of("\\\""), '\\', take(1usize)),
                tag("\""),
            )));

            let mut param_value_parser = alt((string_parser, tag_parser));

            let (input, param_value) = param_value_parser(input)?;

            line_tokens.push(Token::ParamValue(
                param_value,
                colorscheme.param_value_color,
            ));

            *parser_state = ParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
    };
}

pub fn parse_curlman_editor<'a>(
    input: &'a Vec<String>,
    colorscheme: &'a Colorscheme,
) -> IResult<(), Vec<Vec<Token<'a>>>> {
    let mut editor_tokens = Vec::new();
    let mut line_tokens = Vec::new();
    let mut parser_state = ParserState::ExpectingCurl;

    for line in input {
        let mut curr_str: &str = line;
        while !curr_str.is_empty() {
            match parse_curlman_editor_line(curr_str, &mut parser_state, colorscheme) {
                Ok((input, new_tokens)) => {
                    line_tokens.extend(new_tokens);
                    curr_str = input;
                }
                Err(_) => {
                    line_tokens.push(Token::Unknown(curr_str, colorscheme.unknown_color));
                    break;
                }
            }
        }
        editor_tokens.push(std::mem::take(&mut line_tokens));
    }

    Ok(((), editor_tokens))
}

#[cfg(test)]
mod tests {

    use crate::editor::colors::get_default_colorscheme;

    use super::*;

    #[test]
    fn test_request_parser() {
        let input = r#"
            curl http://example.com
            -X POST 
            -H "Authorization: Bearer ${TOKEN}"
        "#;

        let res = parse_curlman_request(input);
        assert!(res.is_ok())
    }

    #[test]
    fn test_editor_parser() {
        let input = vec![
            String::from(""),
            String::from("curl http://example.com"),
            String::from("-H \"Authorization"),
            String::from("-X POST"),
        ];
        let colorscheme = get_default_colorscheme();
        let res = parse_curlman_editor(&input, &colorscheme);
        assert!(res.is_ok());
    }

    #[test]
    fn test_request_file_parser() {
        let input = r#"
            curl http://example.com
            -X GET 
            -H "Authorization: Bearer ${TOKEN}"

            ===
            curl http://example.com
            -X POST 
            -H "Authorization: Bearer ${TOKEN}"
            --data '{"json_is" : "cool", "right" : false}'

        "#;

        let res = parse_curlman_request_file(input);
        assert!(res.is_ok_and(|r| r.1.len() == 2))
    }
}
