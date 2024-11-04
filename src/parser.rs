use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take, take_till, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, none_of, one_of},
    combinator::{map, recognize},
    error::{Error, ErrorKind},
    multi::{many1, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};

use ratatui::style::Color;
use url::Url;

use crate::{
    editor::colors::{EditorColorscheme, JsonOutputColorscheme},
    types::{BodyType, CurlmanRequestParamType, RequestInfo},
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
pub enum Token<'a> {
    Curl(&'a str),
    Url(&'a str),
    ParamKey(&'a str),
    ParamValue(&'a str),
    Whitespace(&'a str),
    Separator(&'a str),
    Unknown(&'a str),
}

impl<'a> Token<'a> {
    pub fn get_str(&self) -> &str {
        match self {
            Token::Curl(text) => text,
            Token::Url(text) => text,
            Token::ParamKey(text) => text,
            Token::ParamValue(text) => text,
            Token::Whitespace(text) => text,
            Token::Separator(text) => text,
            Token::Unknown(text) => text,
        }
    }

    pub fn get_color(&self, colorscheme: &EditorColorscheme) -> Color {
        match self {
            Token::Curl(_) => colorscheme.curl_color,
            Token::Url(_) => colorscheme.url_color,
            Token::ParamKey(_) => colorscheme.param_key_color,
            Token::ParamValue(_) => colorscheme.param_value_color,
            Token::Separator(_) => colorscheme.separator_color,
            Token::Unknown(_) => colorscheme.unknown_color,
            Token::Whitespace(_) => Color::White,
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

pub fn parse_curlman_request_file(input: &str) -> IResult<&str, Vec<RequestInfo>> {
    let (input, requests) = separated_list0(tag("==="), parse_curlman_request)(input)?;
    Ok((input, requests))
}

#[derive(Debug)]
enum RequestParserState {
    ExpectingCurl,
    ExpectingUrl,
    ExpectingParamKey,
    ExpectingParamValue,
}

fn parse_curlman_editor_line<'a, 'b>(
    input: &'a str,
    parser_state: &'b mut RequestParserState,
    colorscheme: &EditorColorscheme,
) -> IResult<&'a str, Vec<Token<'a>>> {
    let mut line_tokens = Vec::new();
    match parser_state {
        RequestParserState::ExpectingCurl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let (input, curl_tag) = tag("curl")(input)?;
            line_tokens.push(Token::Curl(curl_tag));

            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            *parser_state = RequestParserState::ExpectingUrl;
            return Ok((input, line_tokens));
        }

        RequestParserState::ExpectingUrl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let (input, url_str) = take_till(char::is_whitespace)(input)?;
            let url_parse_res: Result<Url, _> = url_str.parse();
            line_tokens.push(if url_parse_res.is_ok() {
                Token::Url(url_str)
            } else {
                Token::Unknown(url_str)
            });

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }
            *parser_state = RequestParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
        RequestParserState::ExpectingParamKey => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let separator_res = tag::<_, &str, nom::error::Error<&str>>("===")(input);

            match separator_res {
                Ok((input, separator)) => {
                    *parser_state = RequestParserState::ExpectingCurl;
                    line_tokens.push(Token::Separator(separator));
                    return Ok((input, line_tokens));
                }
                _ => {}
            };

            let mut param_parser = recognize(pair(
                take_while(|ch: char| ch == '-'),
                take_while(|ch: char| ch.is_ascii_alphanumeric()),
            ));

            let (input, param) = param_parser(input)?;

            line_tokens.push(Token::ParamKey(param));

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            *parser_state = RequestParserState::ExpectingParamValue;
            return Ok((input, line_tokens));
        }
        RequestParserState::ExpectingParamValue => {
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(Token::Whitespace(space));
            }

            let tag_parser = take_while1(|ch: char| ch.is_ascii_alphanumeric());

            let mut param_value_parser = alt((string_parser_global, tag_parser));

            let (input, param_value) = param_value_parser(input)?;

            line_tokens.push(Token::ParamValue(param_value));

            *parser_state = RequestParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
    };
}

pub fn parse_curlman_editor<'a>(
    input: &'a Vec<String>,
    colorscheme: &'a EditorColorscheme,
) -> Vec<Vec<Token<'a>>> {
    let mut editor_tokens = Vec::new();
    let mut line_tokens = Vec::new();
    let mut parser_state = RequestParserState::ExpectingCurl;

    for line in input {
        let mut curr_str: &str = line;
        while !curr_str.is_empty() {
            match parse_curlman_editor_line(curr_str, &mut parser_state, colorscheme) {
                Ok((input, new_tokens)) => {
                    line_tokens.extend(new_tokens);
                    curr_str = input;
                }
                Err(_) => {
                    line_tokens.push(Token::Unknown(curr_str));
                    break;
                }
            }
        }
        editor_tokens.push(std::mem::take(&mut line_tokens));
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

pub fn parse_json_editor(input: &Vec<String>) -> Vec<Vec<JsonToken>> {
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

    use crate::editor::colors::get_default_editor_colorscheme;
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
                body: None
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
        let colorscheme = get_default_editor_colorscheme();
        let res = parse_curlman_editor(&input, &colorscheme);
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
                    headers: HashMap::new(),
                    url: Some("http://example.com".parse::<Url>().unwrap()),
                    method: http::Method::GET,
                    timeout: Duration::from_secs(30),
                    body: None
                },
                RequestInfo {
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
                body: None
            },]
        )
    }
}
