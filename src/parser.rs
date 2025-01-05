use std::time::Duration;

use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take, take_till, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, none_of, one_of},
    combinator::{map, map_res, recognize},
    error::{Error, VerboseError, VerboseErrorKind},
    multi::{many1, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};

use ratatui::style::Color;
use url::Url;

use crate::{
    editor::colors::{EditorColorscheme, JsonOutputColorscheme},
    types::{BodyType, CurlFlag, CurlmanRequestParamType, RequestInfo},
};

fn string_parser_global(input: &str) -> IResult<&str, &str> {
    let double_quoted = tuple((
        tag::<_, _, Error<&str>>("\""),
        escaped(none_of("\\\""), '\\', take(1usize)),
        tag("\""),
    ));
    let single_quoted = tuple((
        tag::<_, _, Error<&str>>("\'"),
        escaped(none_of("\\\'"), '\\', take(1usize)),
        tag("\'"),
    ));
    let mut string_parser = recognize(alt((double_quoted, single_quoted)));
    string_parser(input)
}

#[derive(Debug)]
pub enum CurlmanToken<'a> {
    Curl(&'a str),
    Url(&'a str),
    ParamKey(&'a str),
    ParamValue(&'a str),
    Whitespace(&'a str),
    Separator(&'a str),
    Unknown(&'a str),
}

impl<'a> CurlmanToken<'a> {
    pub fn get_str(&self) -> &str {
        match self {
            CurlmanToken::Curl(text) => text,
            CurlmanToken::Url(text) => text,
            CurlmanToken::ParamKey(text) => text,
            CurlmanToken::ParamValue(text) => text,
            CurlmanToken::Whitespace(text) => text,
            CurlmanToken::Separator(text) => text,
            CurlmanToken::Unknown(text) => text,
        }
    }

    pub fn get_color(&self, colorscheme: &EditorColorscheme) -> Color {
        match self {
            CurlmanToken::Curl(_) => colorscheme.curl_color,
            CurlmanToken::Url(_) => colorscheme.url_color,
            CurlmanToken::ParamKey(_) => colorscheme.param_key_color,
            CurlmanToken::ParamValue(_) => colorscheme.param_value_color,
            CurlmanToken::Separator(_) => colorscheme.separator_color,
            CurlmanToken::Unknown(_) => colorscheme.unknown_color,
            CurlmanToken::Whitespace(_) => Color::White,
        }
    }
}

fn parse_curl_params<'a>(input: &'a str) -> IResult<&'a str, RequestInfo, VerboseError<&str>> {
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

        match param_type_res {
            Ok(param_type) => match param_type {
                CurlmanRequestParamType::Method => {
                    let method_res: Result<Method, _> = value.parse();
                    let Ok(method) = method_res else {
                        return Err(nom::Err::Failure(VerboseError {
                            errors: vec![(
                                value,
                                VerboseErrorKind::Context("Invalid Request Method"),
                            )],
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
                CurlmanRequestParamType::Timeout => {
                    let (_, real_value) = decimal_integer(value).map_err(|_| {
                        nom::Err::Failure(VerboseError {
                            errors: vec![(
                                value,
                                VerboseErrorKind::Context("Invalid Request Timeout Len"),
                            )],
                        })
                    })?;

                    request_info.timeout = Duration::from_secs(real_value);
                }
            },
            Err(_) => {
                let Ok(flag) = param_type.parse::<CurlFlag>() else {
                    return Err(nom::Err::Failure(VerboseError {
                        errors: vec![(
                            param_type,
                            VerboseErrorKind::Context("Invalid Parameter Type"),
                        )],
                    }));
                };

                request_info.flags.push(flag)
            }
        };
    }

    Ok((input, request_info))
}

pub fn parse_curlman_request(input: &str) -> IResult<&str, RequestInfo, VerboseError<&str>> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("curl")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, url_str) = take_till(char::is_whitespace)(input)?;

    let url_res: Result<Url, _> = url_str.parse();

    let Ok(url) = url_res else {
        return Err(nom::Err::Failure(VerboseError {
            errors: vec![(url_str, VerboseErrorKind::Context("Invalid Url"))],
        }));
    };

    if input.is_empty() {
        return Ok(("", RequestInfo::default_with_url(url)));
    }

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

pub fn parse_curlman_request_file(
    input: &str,
) -> IResult<&str, Vec<RequestInfo>, VerboseError<&str>> {
    let (input, requests) = separated_list0(tag("==="), parse_curlman_request)(input)?;
    Ok((input, requests))
}

#[derive(Debug)]
enum RequestParserState<'a> {
    ExpectingCurl,
    ExpectingUrl,
    ExpectingParamKey,
    ExpectingParamValueStart,
    ExpectingParamValueEnd(&'a str),
}

fn parse_curlman_editor_line<'a, 'b>(
    input: &'a str,
    parser_state: &'b mut RequestParserState<'a>,
) -> IResult<&'a str, Vec<CurlmanToken<'a>>> {
    let mut line_tokens = Vec::new();
    match parser_state {
        RequestParserState::ExpectingCurl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }

            let (input, curl_tag) = tag("curl")(input)?;
            line_tokens.push(CurlmanToken::Curl(curl_tag));

            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }

            *parser_state = RequestParserState::ExpectingUrl;
            return Ok((input, line_tokens));
        }

        RequestParserState::ExpectingUrl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }

            let (input, url_str) = take_till(char::is_whitespace)(input)?;
            let url_parse_res: Result<Url, _> = url_str.parse();
            line_tokens.push(if url_parse_res.is_ok() {
                CurlmanToken::Url(url_str)
            } else {
                CurlmanToken::Unknown(url_str)
            });

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }
            *parser_state = RequestParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
        RequestParserState::ExpectingParamKey => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }

            let separator_res = tag::<_, &str, nom::error::Error<&str>>("===")(input);

            match separator_res {
                Ok((input, separator)) => {
                    *parser_state = RequestParserState::ExpectingCurl;
                    line_tokens.push(CurlmanToken::Separator(separator));
                    return Ok((input, line_tokens));
                }
                _ => {}
            };

            let mut param_parser = recognize(pair(
                take_while(|ch: char| ch == '-'),
                take_while(|ch: char| ch.is_ascii_alphanumeric()),
            ));

            let (input, param) = param_parser(input)?;
            line_tokens.push(CurlmanToken::ParamKey(param));
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }
            *parser_state = RequestParserState::ExpectingParamValueStart;
            return Ok((input, line_tokens));
        }
        RequestParserState::ExpectingParamValueStart => {
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanToken::Whitespace(space));
            }
            let tag_parser =
                take_while1::<_, _, Error<&str>>(|ch: char| ch.is_ascii_alphanumeric());

            let param_value_res = tag_parser(input);

            return match param_value_res {
                Ok((input, param_value)) => {
                    line_tokens.push(CurlmanToken::ParamValue(param_value));
                    let (input, space) = multispace0(input)?;
                    if !space.is_empty() {
                        line_tokens.push(CurlmanToken::Whitespace(space));
                    }
                    *parser_state = RequestParserState::ExpectingParamKey;
                    Ok((input, line_tokens))
                }
                Err(_) => {
                    let string_parse_res = string_parser_global(input);
                    match string_parse_res {
                        Ok((input, string)) => {
                            line_tokens.push(CurlmanToken::ParamValue(string));
                            *parser_state = RequestParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => {
                            let (input, string_start) = alt((tag("\'"), tag("\"")))(input)?;
                            line_tokens.push(CurlmanToken::ParamValue(string_start));
                            let (input, space) = multispace0(input)?;
                            if !space.is_empty() {
                                line_tokens.push(CurlmanToken::Whitespace(space));
                            }
                            *parser_state =
                                RequestParserState::ExpectingParamValueEnd(string_start);

                            Ok((input, line_tokens))
                        }
                    }
                }
            };
        }
        RequestParserState::ExpectingParamValueEnd(ref delimiter) => {
            let line_consume_res = is_not::<_, _, nom::error::Error<&str>>(*delimiter)(input);

            match line_consume_res {
                Ok((input, line)) => {
                    line_tokens.push(CurlmanToken::ParamValue(line));

                    let end_string_res = tag::<_, _, nom::error::Error<&str>>(*delimiter)(input);

                    match end_string_res {
                        Ok((input, end)) => {
                            line_tokens.push(CurlmanToken::ParamValue(end));
                            *parser_state = RequestParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => Ok((input, line_tokens)),
                    }
                }
                Err(_) => {
                    let end_string_res = tag::<_, _, nom::error::Error<&str>>(*delimiter)(input);

                    match end_string_res {
                        Ok((input, end)) => {
                            line_tokens.push(CurlmanToken::ParamValue(end));
                            *parser_state = RequestParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => Ok((input, line_tokens)),
                    }
                }
            }
        }
    }
}

pub fn parse_curlman_editor<'a>(input: &'a Vec<String>) -> Vec<Vec<CurlmanToken<'a>>> {
    let mut editor_tokens = Vec::new();
    let mut line_tokens = Vec::new();
    let mut parser_state = RequestParserState::ExpectingCurl;

    for line in input {
        let mut curr_str: &str = line;
        while !curr_str.is_empty() {
            match parse_curlman_editor_line(curr_str, &mut parser_state) {
                Ok((input, new_tokens)) => {
                    line_tokens.extend(new_tokens);
                    curr_str = input;
                }
                Err(_) => {
                    line_tokens.push(CurlmanToken::Unknown(curr_str));
                    break;
                }
            }
        }
        if !line_tokens.is_empty() {
            editor_tokens.push(std::mem::take(&mut line_tokens));
        } else {
            editor_tokens.push(vec![CurlmanToken::Whitespace("")]);
        }
    }

    editor_tokens
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonToken<'editor_repr> {
    ObjectBracket(&'editor_repr str),
    ArrayBracket(&'editor_repr str),
    KeySeparator(&'editor_repr str),
    Identifier(&'editor_repr str),
    ValueSeparator(&'editor_repr str),
    Literal(&'editor_repr str),
    String(&'editor_repr str),
    Whitespace(&'editor_repr str),
    Invalid(&'editor_repr str),
}

impl<'repr> JsonToken<'repr> {
    pub fn get_str(&self) -> &str {
        match self {
            JsonToken::ObjectBracket(text)
            | JsonToken::ArrayBracket(text)
            | JsonToken::KeySeparator(text)
            | JsonToken::Identifier(text)
            | JsonToken::ValueSeparator(text)
            | JsonToken::Literal(text)
            | JsonToken::String(text)
            | JsonToken::Whitespace(text)
            | JsonToken::Invalid(text) => text,
        }
    }
    pub fn get_color(&self, colorscheme: &JsonOutputColorscheme) -> Color {
        match self {
            JsonToken::ObjectBracket(_) => colorscheme.object_bracket_color,
            JsonToken::ArrayBracket(_) => colorscheme.array_bracket_color,
            JsonToken::KeySeparator(_) => colorscheme.name_separator_color,
            JsonToken::ValueSeparator(_) => colorscheme.value_separator_color,
            JsonToken::Literal(_) => colorscheme.literal_color,
            JsonToken::String(_) => colorscheme.string_color,
            JsonToken::Whitespace(_) => Color::White,
            JsonToken::Invalid(_) => colorscheme.invalid_color,
            JsonToken::Identifier(_) => colorscheme.identifier_color,
        }
    }
}

enum JsonParserState {
    AwaitingValue,
    AwaitingValueOrEnd,
    AwaitingValueSeparatorOrEnd,
    AwaitingKeyOrEnd,
    AwaitingKeySeparator,
    AwaitingRecordSeparatorOrEnd,
    Done,
}

#[derive(PartialEq, Debug)]
enum JsonContext {
    Array,
    Object,
}

struct JsonParser {
    current_state: JsonParserState,
    context_stack: Vec<JsonContext>,
}

fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(many1(one_of("0123456789")))(input)
}

fn decimal_integer(input: &str) -> IResult<&str, u64> {
    map_res(recognize(many1(one_of("0123456789"))), |s: &str| {
        s.parse::<u64>()
    })(input)
}

fn parse_json_editor_line<'a>(
    input: &'a str,
    parser: &mut JsonParser,
) -> IResult<&'a str, Vec<JsonToken<'a>>> {
    let mut tokens = Vec::new();

    let (input, space) = multispace0(input)?;

    if !space.is_empty() {
        tokens.push(JsonToken::Whitespace(space));
    }

    let ret = match parser.current_state {
        JsonParserState::AwaitingValue => {
            let (remaining, token) = alt((
                map(tag("{"), |s| JsonToken::ObjectBracket(s)),
                map(tag("["), |s| JsonToken::ArrayBracket(s)),
                map(tag("null"), |s| JsonToken::Literal(s)),
                map(alt((tag("false"), tag("true"))), |s| JsonToken::Literal(s)),
                map(alt((decimal, recognize_float)), |s| JsonToken::Literal(s)),
                map(string_parser_global, |s| JsonToken::String(s)),
            ))(input)?;

            match token {
                JsonToken::ObjectBracket(_) => {
                    parser.context_stack.push(JsonContext::Object);
                    parser.current_state = JsonParserState::AwaitingKeyOrEnd;
                }
                JsonToken::ArrayBracket(_) => {
                    parser.context_stack.push(JsonContext::Array);
                    parser.current_state = JsonParserState::AwaitingValueOrEnd;
                }
                _ => match parser.context_stack.last() {
                    Some(JsonContext::Object) => {
                        parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd;
                    }
                    Some(JsonContext::Array) => {
                        parser.current_state = JsonParserState::AwaitingValueOrEnd;
                    }
                    None => {
                        parser.current_state = JsonParserState::Done;
                    }
                },
            }
            tokens.push(token);
            Ok((remaining, tokens))
        }
        JsonParserState::AwaitingKeySeparator => {
            let (input, name_sep) = tag(":")(input)?;
            tokens.push(JsonToken::KeySeparator(name_sep));
            parser.current_state = JsonParserState::AwaitingValue;
            Ok((input, tokens))
        }
        JsonParserState::AwaitingKeyOrEnd => match tag::<_, _, Error<&str>>("}")(input) {
            Ok((remaining, object_close)) => {
                tokens.push(JsonToken::ObjectBracket(object_close));
                assert_eq!(parser.context_stack.pop(), Some(JsonContext::Object));

                match parser.context_stack.last() {
                    None => {
                        parser.current_state = JsonParserState::Done;
                    }
                    Some(JsonContext::Array) => {
                        parser.current_state = JsonParserState::AwaitingValueSeparatorOrEnd;
                    }
                    Some(JsonContext::Object) => {
                        parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd
                    }
                }

                Ok((remaining, tokens))
            }
            Err(_) => {
                let (input, identifier) = string_parser_global(input)?;
                tokens.push(JsonToken::Identifier(identifier));
                parser.current_state = JsonParserState::AwaitingKeySeparator;
                Ok((input, tokens))
            }
        },
        JsonParserState::AwaitingRecordSeparatorOrEnd => match tag::<_, _, Error<&str>>("}")(input)
        {
            Ok((remaining, object_close)) => {
                tokens.push(JsonToken::ObjectBracket(object_close));
                assert_eq!(parser.context_stack.pop(), Some(JsonContext::Object));
                match parser.context_stack.last() {
                    Some(JsonContext::Array) => {
                        parser.current_state = JsonParserState::AwaitingValueSeparatorOrEnd;
                    }
                    Some(JsonContext::Object) => {
                        parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd;
                    }
                    None => parser.current_state = JsonParserState::Done,
                }
                Ok((remaining, tokens))
            }
            Err(_) => {
                let (input, comma) = tag(",")(input)?;
                tokens.push(JsonToken::ValueSeparator(comma));
                parser.current_state = JsonParserState::AwaitingKeyOrEnd;
                Ok((input, tokens))
            }
        },
        JsonParserState::AwaitingValueOrEnd => match tag::<_, _, Error<&str>>("]")(input) {
            Ok((remaining, array_close)) => {
                tokens.push(JsonToken::ArrayBracket(array_close));
                assert_eq!(parser.context_stack.pop(), Some(JsonContext::Array));
                parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd;

                match parser.context_stack.last() {
                    Some(JsonContext::Array) => {
                        parser.current_state = JsonParserState::AwaitingValueSeparatorOrEnd;
                    }
                    Some(JsonContext::Object) => {
                        parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd;
                    }
                    None => parser.current_state = JsonParserState::Done,
                }

                Ok((remaining, tokens))
            }
            Err(_) => {
                parser.current_state = JsonParserState::AwaitingValue;
                Ok((input, tokens))
            }
        },
        JsonParserState::AwaitingValueSeparatorOrEnd => {
            match tag::<_, _, Error<&str>>("]")(input) {
                Ok((remaining, array_close)) => {
                    tokens.push(JsonToken::ArrayBracket(array_close));
                    assert_eq!(parser.context_stack.pop(), Some(JsonContext::Array));
                    parser.current_state = JsonParserState::AwaitingRecordSeparatorOrEnd;
                    Ok((remaining, tokens))
                }
                Err(_) => {
                    let (input, comma) = tag(",")(input)?;
                    tokens.push(JsonToken::ValueSeparator(comma));
                    parser.current_state = JsonParserState::AwaitingValueOrEnd;
                    Ok((input, tokens))
                }
            }
        }
        JsonParserState::Done => {
            if !input.is_empty() {
                eprintln!("Input not empty {input}");
                ratatui::restore();
                std::process::exit(0);
            }
            Ok(("", Vec::new()))
        }
    };

    ret
}

pub fn parse_request_json(input: &Vec<String>) -> Vec<Vec<JsonToken>> {
    let mut parser = JsonParser {
        current_state: JsonParserState::AwaitingValue,
        context_stack: Vec::new(),
    };

    let mut token_lines = Vec::new();

    for line in input {
        let mut curr_line: &str = line;
        let mut line_tokens = Vec::new();
        while !curr_line.is_empty() {
            match parse_json_editor_line(curr_line, &mut parser) {
                Ok((remaining, new_tokens)) => {
                    curr_line = remaining;
                    line_tokens.extend(new_tokens);
                }
                Err(_) => {
                    line_tokens.push(JsonToken::Invalid(curr_line));
                    break;
                }
            }
        }
        token_lines.push(line_tokens)
    }
    token_lines
}

#[cfg(test)]
mod tests {

    use std::{collections::HashMap, time::Duration};

    use super::*;

    #[test]
    fn test_individual_request_parser() {
        let input = r#"
                    curl http://example.com
                "#;

        let parse_result = parse_curlman_request(input);
        let Ok((_, request)) = parse_result else {
            panic!("Individual request #1 parsing failed : {:?}", parse_result);
        };

        assert_eq!(
            request,
            RequestInfo {
                headers: HashMap::new(),
                url: Some("http://example.com".parse::<Url>().unwrap()),
                method: http::Method::GET,
                timeout: Duration::from_secs(30),
                body: None,
                flags: vec![]
            }
        );
        let input = r#"curl http://example.com"#;
        let parse_result = parse_curlman_request(input);

        let Ok((_, request)) = parse_result else {
            panic!("Individual request #2 parsing failed : {:?}", parse_result);
        };

        assert_eq!(
            request,
            RequestInfo {
                flags: vec![],
                headers: HashMap::new(),
                url: Some("http://example.com".parse::<Url>().unwrap()),
                method: http::Method::GET,
                timeout: Duration::from_secs(30),
                body: None
            }
        );

        let input = r#"
            curl http://example.com
            -X POST
            -H "Authorization: Basic ${TOKEN}"
            --data '{"json_is_awesome":true, "count": 0, "test": [1,2,3]}'
        "#;
        let parse_result = parse_curlman_request(input);
        let Ok((_, request)) = parse_result else {
            panic!("Individual request #3 parsing failed : {:?}", parse_result);
        };
        assert_eq!(
            request,
            RequestInfo {
                flags: vec![],
                headers: HashMap::from([(
                    "Authorization".to_string(),
                    " Basic ${TOKEN}".to_string()
                )]),
                url: Some("http://example.com".parse::<Url>().unwrap()),
                method: http::Method::POST,
                timeout: Duration::from_secs(30),
                body: Some("{\"json_is_awesome\":true, \"count\": 0, \"test\": [1,2,3]}".into()),
            }
        );
    }

    #[test]
    fn test_editor_parser() {
        let input = vec![
            String::from(""),
            String::from("curl http://example.com"),
            String::from("-H \"Authorization"),
            String::from("-X POST"),
        ];
        let res = parse_curlman_editor(&input);
        assert!(!res.is_empty());
    }

    #[test]
    fn test_request_file_parser() {
        let input = r#"
            curl http://example.com

            ===

            curl http://example.com
            -X POST 
            -H "Authorization: Bearer ${TOKEN}"
            --data '{"json_is" : "cool", "right" : false}'"#;

        let file_parse_result = parse_curlman_request_file(input);

        let Ok((_, requests)) = file_parse_result else {
            panic!("Failed to parse request file #1 {:?}", file_parse_result);
        };

        assert_eq!(
            requests,
            vec![
                RequestInfo {
                    flags: vec![],
                    headers: HashMap::new(),
                    url: Some("http://example.com".parse::<Url>().unwrap()),
                    method: http::Method::GET,
                    timeout: Duration::from_secs(30),
                    body: None
                },
                RequestInfo {
                    flags: vec![],
                    headers: HashMap::from([(
                        "Authorization".to_string(),
                        " Bearer ${TOKEN}".to_string()
                    )]),
                    url: Some("http://example.com".parse::<Url>().unwrap()),
                    method: http::Method::POST,
                    timeout: Duration::from_secs(30),
                    body: Some("{\"json_is\" : \"cool\", \"right\" : false}".into()),
                }
            ]
        );

        let input = r#"curl http://example.com"#;

        let file_parse_result = parse_curlman_request_file(input);

        let Ok((_, requests)) = file_parse_result else {
            panic!("Failed to parse request file #2 {:?}", file_parse_result);
        };

        assert_eq!(
            requests,
            vec![RequestInfo {
                headers: HashMap::new(),
                flags: vec![],
                url: Some("http://example.com".parse::<Url>().unwrap()),
                method: http::Method::GET,
                timeout: Duration::from_secs(30),
                body: None
            },]
        );

        let input = r#"
            curl http://example.com
        "#;

        let file_parse_result = parse_curlman_request_file(input);

        let Ok((_, requests)) = file_parse_result else {
            panic!("Failed to parse request file #3 {:?}", file_parse_result);
        };

        assert_eq!(
            requests,
            vec![RequestInfo {
                headers: HashMap::new(),
                url: Some("http://example.com".parse::<Url>().unwrap()),
                method: http::Method::GET,
                timeout: Duration::from_secs(30),
                body: None,
                flags: vec![]
            },]
        )
    }
}
