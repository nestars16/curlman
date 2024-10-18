use nom::{
    bytes::complete::{tag, take_till},
    character::complete::multispace0,
    error::{Error, ErrorKind},
    IResult,
};

use url::{self, Url};

fn parse_request(input: &str) -> IResult<&str, ()> {
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
    dbg!(url);

    Ok((input, ()))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parser() {
        let input = r#"
            curl http://example.com 
        "#;

        let res = parse_request(input);

        assert_eq!(res.unwrap().1, ())
    }
}
