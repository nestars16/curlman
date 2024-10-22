use http::Method;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while},
    character::complete::{multispace0, multispace1},
    error::{Error, ErrorKind},
    multi::separated_list0,
    sequence::{delimited, separated_pair},
    IResult,
};

//TODO
//CONSIDER CURL REDIRECT FLAG
//consider basic auth
//file uploads

use url::{self, Url};

use crate::types::{BodyType, CurlmanRequestParamType, RequestInfo};

fn parse_curl_params(input: &str) -> IResult<&str, RequestInfo> {
    let string_parser = alt((
        delimited(tag("\""), take_till(|ch: char| ch == '"'), tag("\"")),
        delimited(tag("'"), take_till(|ch: char| ch == '\''), tag("'")),
    ));

    let tag_parser = take_while(|ch: char| !ch.is_whitespace());

    let (input, params) = separated_list0(
        multispace1,
        separated_pair(
            take_till(char::is_whitespace),
            multispace1,
            alt((string_parser, tag_parser)),
        ),
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
            CurlmanRequestParamType::Body(BodyType::Json) => {}
        }
    }

    Ok((input, request_info))
}

pub fn parse_curlman_request(input: &str) -> IResult<&str, RequestInfo> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("curl ")(input)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parser() {
        let input = r#"
            curl http://example.com
            -X GET
            -H "Authorization: Bearer ${TOKEN}"
            --data '{"json"  : "lol"}'
        "#;

        let res = parse_curlman_request(input);
        dbg!(&res);
        assert!(res.is_ok())
    }
}
