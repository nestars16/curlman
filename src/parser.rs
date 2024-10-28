use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{char, multispace0, multispace1},
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};

//TODO?
//CONSIDER CURL REDIRECT FLAG?
//consider basic auth?
//file uploads?

use nom_locate::{position, LocatedSpan};
use url::{self, Url};

use crate::types::{BodyType, CurlmanRequestParamType, RequestInfo, RequestInfoFileMetadata};

fn parse_curl_params(input: Span) -> IResult<Span, RequestInfo> {
    let string_parser = alt((
        delimited(char('"'), take_till(|ch: char| ch == '"'), char('"')),
        delimited(char('\''), take_till(|ch: char| ch == '\''), char('\'')),
    ));

    let (input, _) = multispace0(input)?;
    let tag_parser = take_while(|ch: char| ch.is_ascii_alphanumeric());

    let param_parser = preceded(
        take_while(|ch: char| ch == '-'),
        take_while(|ch: char| ch.is_ascii_alphanumeric()),
    );

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

pub fn parse_curlman_request(input: Span) -> IResult<Span, RequestInfo> {
    let (input, start_index) = position(input)?;
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
    let (input, _) = multispace0(input)?;
    let (input, end_index) = position(input)?;

    request_builder.url = Some(url);
    request_builder.file_position = Some(RequestInfoFileMetadata {
        start_index: start_index.location_offset(),
        end_index: end_index.location_offset(),
    });

    Ok((input, request_builder))
}

struct Request {
    info: RequestInfo,
    name: String,
}

type Span<'a> = LocatedSpan<&'a str>;
pub fn parse_curlman_request_file(input: Span) -> IResult<Span, Vec<RequestInfo>> {
    let (input, requests) = separated_list0(tag("==="), parse_curlman_request)(input)?;
    Ok((input, requests))
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

        let res = parse_curlman_request(Span::new(input));
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

        let res = parse_curlman_request_file(Span::new(input));
        assert!(res.is_ok_and(|r| r.1.len() == 2))
    }
}
