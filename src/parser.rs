use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{multispace0, multispace1},
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair, tuple},
    IResult,
};

//TODO?
//CONSIDER CURL REDIRECT FLAG?
//consider basic auth?
//file uploads?

use url::{self, Url};

use crate::types::{BodyType, CurlmanRequestParamType, RequestInfo};

fn parse_curl_params(input: &str) -> IResult<&str, RequestInfo> {
    let string_parser = alt((
        delimited(tag("\""), take_till(|ch: char| ch == '"'), tag("\"")),
        delimited(tag("'"), take_till(|ch: char| ch == '\''), tag("'")),
    ));

    let tag_parser = take_while(|ch: char| !ch.is_whitespace());

    let param_separator = tuple((multispace1, take_till(|ch: char| ch == '-')));

    let (input, params) = separated_list0(
        param_separator,
        separated_pair(
            take_till(char::is_whitespace),
            multispace1,
            alt((string_parser, tag_parser)),
        ),
    )(input)?;

    dbg!(&params);

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
            CurlmanRequestParamType::Body(BodyType::Json) => {}
        }
    }

    Ok((input, request_info))
}

pub fn parse_curlman_request(input: &str) -> IResult<&str, RequestInfo> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("curl")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, url_str) = take_till(char::is_whitespace)(input)?;
    let url_res: Result<Url, _> = url_str.parse();

    let Ok(url) = url_res else {
        return Err(nom::Err::Failure(Error {
            input,
            code: ErrorKind::IsNot,
        }));
    };

    let (input, _) = multispace0(input)?;
    let (input, mut request_builder) = parse_curl_params(input)?;

    request_builder.url = Some(url);
    Ok((input, request_builder))
}

struct Request {
    info: RequestInfo,
    name: String,
}

pub fn parse_curlman_request_file(input: &str) -> IResult<&str, Vec<RequestInfo>> {
    let separator = pair(multispace0, tag("==="));

    let (input, requests) = separated_list0(separator, parse_curlman_request)(input)?;

    Ok((input, requests))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parser() {
        let input = r#"
            curl http://example.com
            -X GET
            -H "Authorization: Bearer ${TOKEN}"
    "#;

        let res = parse_curlman_request(input);

        assert!(res.is_ok())
    }

    #[test]
    fn test_file_parser() {
        let input = r#"

            curl http://example.com
            -X POST 
            -H "Authorization: Bearer ${TOKEN}"
            --data '{"json"  : "lol"}'


            ===
            curl http://example.com
            -X GET 
            -H "Authorization: Bearer bear"

            ===
        "#;

        let res = parse_curlman_request_file(input);

        assert!(res.is_ok())
    }
}
