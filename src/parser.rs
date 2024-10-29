use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, recognize},
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair},
    IResult,
};

//TODO?
//CONSIDER CURL REDIRECT FLAG?
//consider basic auth?
//file uploads?

use url::{self, Url};

use crate::types::{BodyType, CurlmanRequestParamType, RequestInfo};

#[derive(Debug)]
pub enum Token<'a> {
    Curl(&'a str),
    Url(&'a str),
    ParamKey(&'a str),
    ParamValue(&'a str),
    Whitespace(&'a str),
}

fn parse_curl_params<'a>(
    input: &'a str,
    mut tokens: Vec<Token<'a>>,
) -> IResult<&'a str, (RequestInfo, Vec<Token<'a>>)> {
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
        tokens.push(Token::ParamKey(param_type));
        tokens.push(Token::ParamValue(value));

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

    Ok((input, (request_info, tokens)))
}

pub fn parse_curlman_request(input: &str) -> IResult<&str, (RequestInfo, Vec<Token>)> {
    let mut tokens = Vec::new();
    let (input, space) = multispace0(input)?;
    tokens.push(Token::Whitespace(space));

    let (input, curl_str) = tag("curl")(input)?;
    tokens.push(Token::Curl(curl_str));

    let (input, space) = multispace0(input)?;
    tokens.push(Token::Whitespace(space));

    let (input, url_str) = take_till(char::is_whitespace)(input)?;
    tokens.push(Token::Url(url_str));
    let url_res: Result<Url, _> = url_str.parse();
    let Ok(url) = url_res else {
        return Err(nom::Err::Failure(Error {
            input,
            code: ErrorKind::IsNot,
        }));
    };

    let (input, space) = multispace0(input)?;
    tokens.push(Token::Whitespace(space));

    let (input, (mut request_builder, mut tokens)) = parse_curl_params(input, tokens)?;
    let (input, space) = multispace0(input)?;
    tokens.push(Token::Whitespace(space));
    request_builder.url = Some(url);
    Ok((input, (request_builder, tokens)))
}

struct Request {
    info: RequestInfo,
    name: String,
}

pub fn parse_curlman_request_file(input: &str) -> IResult<&str, (Vec<RequestInfo>, Vec<Token>)> {
    let (input, requests_and_tokens) = separated_list0(tag("==="), parse_curlman_request)(input)?;

    let mut requests = Vec::with_capacity(requests_and_tokens.len());
    let mut all_tokens = Vec::with_capacity(requests_and_tokens.len());

    for (request, tokens) in requests_and_tokens {
        requests.push(request);
        all_tokens.extend(tokens);
    }

    Ok((input, (requests, all_tokens)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let input = r#"
            curl http://example.com
            -X POST 
            -H "Authorization: Bearer ${TOKEN}"
            --data '{"json_is" : "cool", "right" : false}'
        "#;

        let res = parse_curlman_request(input);
        dbg!(&res);
        assert!(res.is_ok())
    }

    #[test]
    fn test_file_parser() {
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
        assert!(res.is_ok_and(|r| r.1 .0.len() == 2))
    }
}
