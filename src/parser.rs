use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take, take_till, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, none_of},
    combinator::recognize,
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};

//TODO?
//CONSIDER CURL REDIRECT FLAG?
//consider basic auth?
//file uploads?

use ratatui::{
    style::{Color, Stylize},
    text::Span,
};
use serde_json::Value;
use url::{self, form_urlencoded::parse, Url};

use crate::{
    editor::colors::{EditorColorscheme, JsonOutputColorscheme},
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
            line_tokens.push(Token::Curl(curl_tag, colorscheme.curl_color));

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
                Token::Url(url_str, colorscheme.url_color)
            } else {
                Token::Unknown(url_str, colorscheme.unknown_color)
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

            *parser_state = RequestParserState::ExpectingParamValue;
            return Ok((input, line_tokens));
        }
        RequestParserState::ExpectingParamValue => {
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
                    line_tokens.push(Token::Unknown(curr_str, colorscheme.unknown_color));
                    break;
                }
            }
        }
        editor_tokens.push(std::mem::take(&mut line_tokens));
    }

    editor_tokens
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonToken {
    ObjectBracket(String),
    ArrayBracket(String),
    NameSeparator(String),
    ValueSeparator(String),
    Literal(String),
    String(String),
    Whitespace(String),
}

impl JsonToken {
    pub fn get_color(&self, colorscheme: &JsonOutputColorscheme) -> Color {
        match self {
            JsonToken::ObjectBracket(_) => colorscheme.object_bracket_color,
            JsonToken::ArrayBracket(_) => colorscheme.array_bracket_color,
            JsonToken::NameSeparator(_) => colorscheme.name_separator_color,
            JsonToken::ValueSeparator(_) => colorscheme.value_separator_color,
            JsonToken::Literal(_) => colorscheme.literal_color,
            JsonToken::String(_) => colorscheme.string_color,
            JsonToken::Whitespace(_) => Color::White,
        }
    }
}

pub fn parse_json_value(input: &Value, indent_level: usize) -> Vec<Vec<JsonToken>> {
    let mut lines = Vec::new();

    match input {
        Value::Object(map) => {
            // First line: opening bracket
            lines.push(vec![JsonToken::ObjectBracket("{".to_string())]);
            // Process each key-value pair
            for (i, (key, val)) in map.iter().enumerate() {
                let mut line = Vec::new();
                line.push(JsonToken::Whitespace(" ".repeat(indent_level + 1)));
                line.push(JsonToken::String(format!("\"{}\"", key)));
                line.push(JsonToken::NameSeparator(": ".to_string()));
                let nested_lines = parse_json_value(val, indent_level + 1);

                line.extend(nested_lines[0].clone());
                lines.push(line); // Add the current line to `lines`
                                  // Append remaining lines of nested element, if any
                for nested_line in nested_lines.into_iter().skip(1) {
                    lines.push(nested_line);
                }
                // Add a comma at the end of the last line for this element, if needed
                if i < map.len() - 1 {
                    lines
                        .last_mut()
                        .unwrap()
                        .push(JsonToken::ValueSeparator(",".to_string()));
                }
            }

            // Last line: closing bracket
            let mut closing_line = vec![JsonToken::Whitespace(" ".repeat(indent_level))];
            closing_line.push(JsonToken::ObjectBracket("}".to_string()));
            lines.push(closing_line);
        }
        Value::Array(arr) => {
            // First line: opening bracket
            lines.push(vec![JsonToken::ArrayBracket("[".to_string())]);

            // Process each array element
            for (i, val) in arr.iter().enumerate() {
                let mut line = Vec::new();
                line.push(JsonToken::Whitespace(" ".repeat(indent_level + 1)));

                let nested_lines = parse_json_value(val, indent_level + 1);

                // Add the first line of the nested element to the current line
                line.extend(nested_lines[0].clone());
                lines.push(line); // Add the current line to `lines`

                // Append remaining lines of nested element, if any
                for nested_line in nested_lines.into_iter().skip(1) {
                    lines.push(nested_line);
                }

                // Add a comma at the end of the last line for this element, if needed
                if i < arr.len() - 1 {
                    lines
                        .last_mut()
                        .unwrap()
                        .push(JsonToken::ValueSeparator(",".to_string()));
                }
            }

            // Last line: closing bracket
            let mut closing_line = vec![JsonToken::Whitespace(" ".repeat(indent_level))];
            closing_line.push(JsonToken::ArrayBracket("]".to_string()));
            lines.push(closing_line);
        }
        Value::String(s) => lines.push(vec![JsonToken::String(format!("\"{}\"", s))]),
        Value::Number(n) => lines.push(vec![JsonToken::Literal(n.to_string())]),
        Value::Bool(b) => lines.push(vec![JsonToken::Literal(b.to_string())]),
        Value::Null => lines.push(vec![JsonToken::Literal("null".to_string())]),
    }

    lines
}

#[cfg(test)]
mod tests {

    use std::{collections::HashMap, time::Duration};

    use crate::editor::colors::get_default_colorscheme;

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
        let colorscheme = get_default_colorscheme();
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

    #[test]
    fn test_json_parsing() {
        let json_str = "{\"name\": \"Nestor\", \"age\": 30}";
        let input: Value = serde_json::from_str(json_str).unwrap();
        let lines = parse_json_value(&input, 0);
        assert_eq!(
            lines,
            vec![
                vec![JsonToken::ObjectBracket("{".to_string())],
                vec![
                    JsonToken::Whitespace(" ".to_string()),
                    JsonToken::String("\"age\"".to_string()),
                    JsonToken::NameSeparator(": ".to_string()),
                    JsonToken::Literal("30".to_string()),
                    JsonToken::ValueSeparator(",".to_string()),
                ],
                vec![
                    JsonToken::Whitespace(" ".to_string()),
                    JsonToken::String("\"name\"".to_string()),
                    JsonToken::NameSeparator(": ".to_string()),
                    JsonToken::String("\"Nestor\"".to_string()),
                ],
                vec![
                    JsonToken::Whitespace("".to_string()),
                    JsonToken::ObjectBracket("}".to_string()),
                ],
            ]
        );
    }
}
