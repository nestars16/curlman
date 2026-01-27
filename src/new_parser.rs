use std::ops::Range;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take, take_while1},
    character::complete::{multispace1, none_of},
    combinator::{map, opt, recognize, value},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{pair, preceded, terminated, tuple},
    IResult, Slice,
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
    let double_quoted = recognize(tuple((
        tag::<_, _, _>("\""),
        escaped(none_of("\\\""), '\\', take(1usize)),
        tag("\""),
    )));

    let single_quoted = recognize(tuple((
        tag::<_, _, _>("\'"),
        escaped(none_of("\\\'"), '\\', take(1usize)),
        tag("\'"),
    )));

    let unquoted = take_while1(|ch: char| !ch.is_whitespace() && ch != '"' && ch != '\'');

    let word_parser = recognize(many1(alt((double_quoted, single_quoted, unquoted))));

    map(word_parser, |tag: Span| {
        CurlmanToken::from_span(tag, CurlmanTokenType::Word)
    })(input)
}

pub fn parse_string(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    let double_quoted = recognize(tuple((
        tag::<_, _, _>("\""),
        escaped(none_of("\\\""), '\\', take(1usize)),
        tag("\""),
    )));
    let single_quoted = recognize(tuple((
        tag::<_, _, _>("\'"),
        escaped(none_of("\\\'"), '\\', take(1usize)),
        tag("\'"),
    )));

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

    macro_rules! push_flag {
        ($flag_type:expr, $idx:expr, $value:expr, $value_token_idx:expr) => {{
            let needs_value = $flag_type.needs_value();
            let has_value = $value.is_some();

            out.push(CurlmanIr::Flag {
                flag_token_idx: $idx,
                value_token_idx: $value_token_idx,
                value: CurlFlag::new($flag_type, $value),
            });

            state = if needs_value && !has_value {
                State::LookingForFlagValue
            } else {
                State::LookingForUrl
            };
        }};
    }

    for (idx, token) in tokens.iter().enumerate() {
        let CurlmanToken {
            token_type, lexeme, ..
        } = token;

        match state {
            State::LookingForFlagValue
                if *token_type == CurlmanTokenType::Flag
                    || *token_type == CurlmanTokenType::ShortFlag =>
            {
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
            State::LookingForUrl
                if *token_type == CurlmanTokenType::Word
                    || *token_type == CurlmanTokenType::String =>
            {
                let url_lexeme = if *token_type == CurlmanTokenType::String {
                    &lexeme[1..lexeme.len() - 1] // lets hope its not a utf-8 character!
                } else {
                    lexeme
                };

                let url = Url::parse(url_lexeme).map_err(|e| {
                    crate::error::parser::Error::InvalidUrl(format!("Invalid url: {e:?}"))
                })?;

                out.push(CurlmanIr::Url {
                    token_idx: idx,
                    value: url,
                })
            }
            State::LookingForUrl => {
                if let Ok(flag_type) = lexeme.parse::<CurlFlagType>() {
                    push_flag!(flag_type, idx, Option::<String>::None, None);
                } else if token.token_type == CurlmanTokenType::ShortFlag {
                    //there are two possible scenarios here
                    //1. the short flag takes an argument and it is together ie. -XPOST
                    //2. its a combination of short flags in succession
                    // my reasoning for this is, if there didnt exist a CurlFlagType for this
                    // string its because it meets one of the two above criteria
                    // so we need to strip the first flag and check which of the two outcomes
                    match token.lexeme.slice(0..2).parse::<CurlFlagType>() {
                        Ok(flag_type) => {
                            let inline_value = token.lexeme.slice(2..);
                            match (flag_type.needs_value(), inline_value.is_empty()) {
                                (true, false) => {
                                    push_flag!(
                                        flag_type,
                                        idx,
                                        Some(inline_value.to_string()),
                                        Some(idx)
                                    );
                                }
                                (true, true) => {
                                    push_flag!(flag_type, idx, Option::<String>::None, None);
                                }
                                (false, _) => {
                                    push_flag!(flag_type, idx, Option::<String>::None, None);

                                    for ch in token.lexeme.slice(2..).chars() {
                                        let short = format!("-{ch}");
                                        let next_flag =
                                            short.parse::<CurlFlagType>().map_err(|_| {
                                                crate::error::parser::Error::InvalidFlag(
                                                    short.to_string(),
                                                )
                                            })?;

                                        if next_flag.needs_value() {
                                            return Err(
                                                crate::error::parser::Error::InvalidFlagValue(
                                                    format!(
                                                        "Unexpected value flag in short flag chain: {short}"
                                                    ),
                                                ),
                                            );
                                        }
                                        push_flag!(next_flag, idx, Option::<String>::None, None);
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            return Err(crate::error::parser::Error::InvalidFlagValue(format!(
                                "Expected flag value and got token: {token:?}",
                            )))
                        }
                    }
                }
            }
        }
    }

    Ok(out)
}

#[cfg(test)]
mod tests {
    use crate::new_parser::CurlmanTokenType::*;
    use crate::new_parser::{lex_curlman_request, parse_tokens_into_ir, CurlmanToken};

    #[test]
    fn test_lexer() {
        let inputs = vec![
            /*
            r#"
            curl --request GET \
            --url 'https://api.x.com/2/tweets/search/recent?query=from%3Atwitterdev&tweet.fields=public_metrics' \
            --header 'Authorization: Bearer $BEARER_TOKEN'
            "#,
            r#"
            curl --request POST \
            --url https://api.sendgrid.com/v3/mail/send \
            --header 'Authorization: Bearer YOUR_API_KEY' \
            --header 'Content-Type: application/json' \
            --data '{"personalizations":[{"to":[{"email":"recipient@example.com"}]}],"from":{"email":"sender@example.com"},"subject":"Hello, World!","content":[{"type":"text/plain","value":"Heya!"}]}'
            "#,
            r#"
            curl -X POST \
            -H 'Authorization: Bearer xoxb-1234-56789abcdefghijklmnop' \
            -H 'Content-type: application/json' \
            --data '{"channel":"C123ABC456","text":"Hello","attachments":[{"text":"Who should win?","actions":[{"name":"winners_list","text":"Who should win?","type":"select","data_source":"users"}]}]}' \
            https://slack.com/api/chat.postMessage
            "#,
            r#"
            curl -X POST "https://slack.com/api/apps.connections.open" \
              -H "Content-type: application/x-www-form-urlencoded" \
              -H "Authorization: Bearer xapp-1-123"
            "#,
            r#"
            curl --request POST \
              --url "https://api.github.com/app/installations/INSTALLATION_ID/ACCESS_TOKENS" \
              --header "Accept: application/vnd.github+json" \
              --header "Authorization: Bearer {jwt}" \
              --header "Content-Type: application/json" \
              --data '{ "repository_ids": [321], "permissions": { "deployments": "write" } }'
              "#,
            r#"
            curl -L \
              -X POST \
              -u "api_key:your-password" \
              -H "Content-Type: application/json" \
              https://hostname.com/manage/v1/config/apply \
              -d '{"run_id":"d34db33f"}'
              "#,
            r#"
            curl -L \
              -X POST \
              -u "api_key:your-password" \
              -H "Content-Type: multipart/form-data" \
              https://hostname.com/manage/v1/config/init \
              --form 'license=@enterprise.ghl' \
              --form 'password=provide-password-here!'
              "#,

            r#"
            curl -XPOST https://api.twilio.com/2010-04-01/Accounts/ACXXXXXXXXXX/Messages.json \
              --data-urlencode "To=+13105555555" \
              --data-urlencode "From=+12125551234" \
              --data-urlencode "MediaUrl=https://demo.twilio.com/owl.png" \
              --data-urlencode "Body=Hello from my Twilio line!" \
              -u ACXXXXXXXXXX:your_auth_token
            "#,
            r#"
            curl https://taskrouter.twilio.com/v1/Workspaces/WorkspaceSid/Tasks \
              --data-urlencode Attributes='{"selected_language": "es"}' \
              -d WorkflowSid={WorkflowSid} \
              -u {AccountSid}:{AuthToken}
            "#,
            r#"
            curl -XGET https://taskrouter.twilio.com/v1/Workspaces/id/TaskQueues \
              --data-urlencode EvaluateWorkerAttributes='{"tech_support_skill": "6"}' \
              -u {AccountSid}:{AuthToken}
            "#,
            r#"
            curl -X POST https://login.microsoftonline.com/id/oauth2/v2.0/token \
              -d 'client_id={web-app-calls-web-api_application_client_id}' \
              -d 'api://{web_API_application_client_id}/Forecast.Read' \
              -d 'code={authorization_code}&session_state={web-app-calls-web-api_application_client_id}' \
              -d 'redirect_uri=http://localhost' \
              -d 'grant_type=authorization_code' \
              -d 'client_secret={client_secret}'
            "#,
            r#"
            curl --user "APITest\API.User" --request GET \
              https://secure.p03.eloqua.com/api/REST/1.0/data/contacts?count=2
            "#,
            r#"
            curl --user "APITest\API.User" --request DELETE \
              https://secure.p03.eloqua.com/api/REST/1.0/data/contact/1
            "#,
            r#"
            curl --user "APITest\API.User" --header "Content-Type: application/json" --request POST \
              --data '{"emailAddress":"george.washington@america.com"}' \
              https://secure.p03.eloqua.com/api/REST/1.0/data/contact
            "#,
            r#"
            curl --user "APITest\API.User" --header "Content-Type: application/json" --request POST \
              --data "@apitest.json" \
              https://secure.p03.eloqua.com/api/bulk/2.0/contacts/imports/30/data
            "#,
            r#"
            curl --user "APITest\API.User" --header "Content-Type: text/csv" --request POST \
              --data-binary "@apitest.csv" \
              https://secure.p03.eloqua.com/api/bulk/2.0/contacts/imports/30/data
            "#,

            r#"
            curl --request PUT \
              "https://api.cloudflare.com/client/v4/zones/id/firewall/rules" \
              --header "X-Auth-Email: <EMAIL>" \
              --header "X-Auth-Key: <API_KEY>" \
              --header "Content-Type: application/json" \
              --data '[{"id":"...","paused":false,"description":"...","action":"block","filter":{"id":"...","expression":"..."} }]'
            "#,
            r#"
            curl https://api.cloudflare.com/client/v4/accounts/id\
              --header "Authorization: Bearer <API_TOKEN>" \
              --header "R2-Access-Key-Id: <R2_ACCESS_KEY_ID>" \
              --header "R2-Secret-Access-Key: <R2_SECRET_ACCESS_KEY>" \
              --header "Content-Type: application/json" \
              --data-raw '{ "start":"2022-08-16T20:30:00Z","end":"2022-08-16T20:31:00","bucket":"cloudflare-logs" }'
            "#,
            */
            /*
            r#"
            curl --request POST 'http://localhost:8787/cdn-cgi/handler/email' \
              --url-query 'from=sender@example.com' \
              --url-query 'to=recipient@example.com' \
              --header 'Content-Type: application/json' \
              --data-raw 'Received: from smtp.example.com (127.0.0.1)
            by cloudflare-email.com (unknown) id 4fwwffRXOpyR
            for <recipient@example.com>; Tue, 27 Aug 2024 15:50:20 +0000
            From: "John" <sender@example.com>
            To: recipient@example.com
            Subject: Testing Email Workers Local Dev
            Hi there'
            "#,
            r#"
            curl "https://api.openai.com/v1/responses" \
              -H "Content-Type: application/json" \
              -H "Authorization: Bearer $OPENAI_API_KEY" \
              -d '{"model":"gpt-5","input":"Hello!"}'
            "#,
            r#"
            curl -X DELETE https://api.openai.com/v1/organization/users/user_abc \
              -H "Authorization: Bearer $OPENAI_ADMIN_KEY" \
              -H "Content-Type: application/json"
            "#,
            r#"
            curl --request POST \
              --url https://api.cloudflare.com/client/v4/accounts/id/ai/v1/chat/completions \
              --header "Authorization: Bearer {api_token}" \
              --header "Content-Type: application/json" \
              --data '{ "model":"@cf/meta/llama-3.1-8b-instruct","messages":[{"role":"user","content":"Hello!"}] }'
            "#,
            r#"
            curl -X POST 'https://graph.microsoft.com/v1.0/servicePrincipals/id/synchronization/jobs' \
              --header 'Content-Type: application/json' \
              --header 'Authorization: Bearer <accessToken>' \
              --data-raw '{ "templateId": "scim" }'
            "#,
            r#"
            curl -X POST 'https://api-m.sandbox.paypal.com/v1/customer/disputes' \
              -H 'Authorization: Bearer <ACCESS TOKEN>' \
              -H 'Content-Type: multipart/related' \
              -H 'PayPal-Auth-Assertion: <JWT TOKEN>' \
              -F 'input={"disputed_transactions":[{"buyer_transaction_id":<BUYER TRANSACTION ID>}],"reason":"MERCHANDISE_OR_SERVICE_NOT_RECEIVED","dispute_amount":{"currency_code":"USD","value":"7.50"}}; type=application/json'
            "#,
            */
        ]
        .into_iter()
        .map(|i: &'static str| lex_curlman_request(i.into()).map(|res| res.1))
        .collect::<Vec<_>>();

        let expected = vec![];

        assert_eq!(inputs, expected)
    }

    #[test]
    fn test_ir_parsing() {
        let inputs = vec![
            r#"curl -kLN -X POST 'https://google.com' \
              --header 'Content-Type: application/json' \
              --header 'Authorization: Bearer <accessToken>' \
              --data-raw '{ "templateId": "scim" }'"#,
        ]
        .into_iter()
        .map(|i| lex_curlman_request(i.into()).map(|res| parse_tokens_into_ir(&res.1)))
        .collect::<Vec<_>>();

        let expected = vec![];

        assert_eq!(inputs, expected);
    }
}
