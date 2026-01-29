use http::Request;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take, take_till, take_while, take_while1},
    character::complete::{multispace0, multispace1, none_of, one_of},
    combinator::{map, opt, recognize, value},
    error::{context, Error, VerboseError},
    multi::{many0, many1},
    number::complete::recognize_float,
    sequence::{pair, preceded, terminated, tuple},
    IResult, Slice,
};

use ratatui::style::Color;

use crate::editor::colors::{EditorColorscheme, JsonOutputColorscheme};
use crate::types::{
    CurlFlag, CurlFlagType, CurlmanIr, CurlmanToken, CurlmanTokenType, EditorParserState,
    RequestInfo, Span,
};

use crate::error::parser;
use crate::error::parser::{ErrorKind, ErrorStage};

use url::Url;

fn error_from_token(
    token: &CurlmanToken,
    kind: ErrorKind,
    message: String,
    context: Vec<String>,
) -> parser::Error {
    parser::Error::new(
        kind,
        ErrorStage::Ir,
        token.span.location_line() as usize,
        token.span.get_column(),
        message,
        context,
    )
}

fn error_from_span(
    span: Span,
    kind: ErrorKind,
    stage: ErrorStage,
    message: String,
    context: Vec<String>,
) -> parser::Error {
    parser::Error::new(
        kind,
        stage,
        span.location_line() as usize,
        span.get_column(),
        message,
        context,
    )
}

fn error_from_nom(input: &str, err: nom::Err<VerboseError<Span>>) -> parser::Error {
    match err {
        nom::Err::Incomplete(_) => error_from_span(
            Span::new(input).slice(input.len()..),
            ErrorKind::InvalidRequest("Incomplete input".to_string()),
            ErrorStage::Lexing,
            "Incomplete input".to_string(),
            Vec::new(),
        ),
        nom::Err::Error(verbose) | nom::Err::Failure(verbose) => {
            let mut context = Vec::new();
            let mut last_span: Option<Span> = None;

            for (span, kind) in &verbose.errors {
                last_span = Some(*span);
                if let nom::error::VerboseErrorKind::Context(ctx) = kind {
                    context.push((*ctx).to_string());
                }
            }

            let message = if context.is_empty() {
                "Invalid request".to_string()
            } else {
                format!("Invalid request: {}", context.join(" > "))
            };

            if let Some(span) = last_span {
                error_from_span(
                    span,
                    ErrorKind::InvalidRequest(message.clone()),
                    ErrorStage::Lexing,
                    message,
                    context,
                )
            } else {
                error_from_span(
                    Span::new(input).slice(input.len()..),
                    ErrorKind::InvalidRequest(message.clone()),
                    ErrorStage::Lexing,
                    message,
                    context,
                )
            }
        }
    }
}

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

pub fn parse_short_flag(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    context(
        "short flag",
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
        "long flag",
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

    let word_parser = context(
        "word",
        recognize(many1(alt((double_quoted, single_quoted, unquoted)))),
    );

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

    let string_parser = context("string", recognize(alt((double_quoted, single_quoted))));

    map(string_parser, |tag: Span| {
        CurlmanToken::from_span(tag, CurlmanTokenType::String)
    })(input)
}

pub fn parse_value(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    context("value", alt((parse_string, parse_word)))(input)
}

pub fn parse_token(input: Span) -> IResult<Span, CurlmanToken, VerboseError<Span>> {
    context(
        "token",
        alt((parse_short_flag, parse_long_flag, parse_value)),
    )(input)
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

    context(
        "whitespace",
        value((), many0(alt((value((), multispace1), line_cont)))),
    )(input)
}

pub fn lex_curlman_request(input: Span) -> IResult<Span, Vec<CurlmanToken>, VerboseError<Span>> {
    let (input, _) = bash_ws0(input)?;
    let (input, _) = context("curl tag", tag("curl"))(input)?;
    let (input, tokens) = context(
        "request",
        preceded(bash_ws0, many0(terminated(parse_token, bash_ws0))),
    )(input)?;

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
                let message = format!("Expected flag value and got token: {token:?}");
                return Err(error_from_token(
                    token,
                    ErrorKind::InvalidFlagValue(message.clone()),
                    message,
                    Vec::new(),
                ));
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
                        let message =
                            format!("Expected flag value but no flag was provided beforehand");
                        return Err(error_from_token(
                            token,
                            ErrorKind::InvalidFlagValue(message.clone()),
                            message,
                            Vec::new(),
                        ));
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
                    let message = format!("Invalid url: {e:?}");
                    error_from_token(
                        token,
                        ErrorKind::InvalidUrl(message.clone()),
                        message,
                        Vec::new(),
                    )
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
                                                let message = format!("Invalid flag: {short}");
                                                error_from_token(
                                                    token,
                                                    ErrorKind::InvalidFlag(message.clone()),
                                                    message,
                                                    Vec::new(),
                                                )
                                            })?;

                                        if next_flag.needs_value() {
                                            let message = format!(
                                                "Unexpected value flag in short flag chain: {short}"
                                            );
                                            return Err(error_from_token(
                                                token,
                                                ErrorKind::InvalidFlagValue(message.clone()),
                                                message,
                                                Vec::new(),
                                            ));
                                        }
                                        push_flag!(next_flag, idx, Option::<String>::None, None);
                                    }
                                }
                            }
                        }
                        Err(_) => {
                            let message = format!("Expected flag value and got token: {token:?}");
                            return Err(error_from_token(
                                token,
                                ErrorKind::InvalidFlagValue(message.clone()),
                                message,
                                Vec::new(),
                            ));
                        }
                    }
                }
            }
        }
    }

    Ok(out)
}

pub fn parse_curlman_request(request_str: &str) -> Result<RequestInfo, parser::Error> {
    let (_, tokens) =
        lex_curlman_request(request_str.into()).map_err(|err| error_from_nom(request_str, err))?;
    let ir_repr = parse_tokens_into_ir(&tokens)?;

    let mut output = RequestInfo::default();

    for ir_token in ir_repr {
        ir_token
    }

    Ok(output)
}

pub fn parse_curlman_request_file(file_contents: &str) -> Result<Vec<RequestInfo>, parser::Error> {
    file_contents
        .split("===")
        .map(|r| parse_curlman_request(r))
        .collect()
}

#[derive(Debug)]
pub enum CurlmanColorToken<'a> {
    Curl(&'a str),
    Url(&'a str),
    ParamKey(&'a str),
    ParamValue(&'a str),
    Whitespace(&'a str),
    EnvVariable(&'a str),
    Separator(&'a str),
    Unknown(&'a str),
}

impl<'a> CurlmanColorToken<'a> {
    pub fn get_str(&self) -> &str {
        match self {
            CurlmanColorToken::Curl(text) => text,
            CurlmanColorToken::Url(text) => text,
            CurlmanColorToken::ParamKey(text) => text,
            CurlmanColorToken::ParamValue(text) => text,
            CurlmanColorToken::Whitespace(text) => text,
            CurlmanColorToken::Separator(text) => text,
            CurlmanColorToken::Unknown(text) => text,
            CurlmanColorToken::EnvVariable(text) => text,
        }
    }

    pub fn get_color(&self, colorscheme: &EditorColorscheme) -> Color {
        match self {
            CurlmanColorToken::Curl(_) => colorscheme.curl_color,
            CurlmanColorToken::Url(_) => colorscheme.url_color,
            CurlmanColorToken::ParamKey(_) => colorscheme.param_key_color,
            CurlmanColorToken::ParamValue(_) => colorscheme.param_value_color,
            CurlmanColorToken::Separator(_) => colorscheme.separator_color,
            CurlmanColorToken::Unknown(_) => colorscheme.unknown_color,
            CurlmanColorToken::Whitespace(_) => Color::White,
            CurlmanColorToken::EnvVariable(_) => Color::White,
        }
    }
}

fn parse_curlman_editor_line<'a, 'b>(
    input: &'a str,
    parser_state: &'b mut EditorParserState<'a>,
) -> IResult<&'a str, Vec<CurlmanColorToken<'a>>> {
    let mut line_tokens = Vec::new();
    match parser_state {
        EditorParserState::ExpectingCurl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }

            let (input, curl_tag) = tag("curl")(input)?;
            line_tokens.push(CurlmanColorToken::Curl(curl_tag));

            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }

            *parser_state = EditorParserState::ExpectingUrl;
            return Ok((input, line_tokens));
        }

        EditorParserState::ExpectingUrl => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }

            let (input, url_str) = take_till(char::is_whitespace)(input)?;
            let url_parse_res: Result<Url, _> = url_str.parse();
            line_tokens.push(if url_parse_res.is_ok() {
                CurlmanColorToken::Url(url_str)
            } else {
                CurlmanColorToken::Unknown(url_str)
            });

            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }
            *parser_state = EditorParserState::ExpectingParamKey;

            return Ok((input, line_tokens));
        }
        EditorParserState::ExpectingParamKey => {
            let (input, space) = multispace0(input)?;

            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }

            let separator_res = tag::<_, &str, nom::error::Error<&str>>("===")(input);

            match separator_res {
                Ok((input, separator)) => {
                    *parser_state = EditorParserState::ExpectingCurl;
                    line_tokens.push(CurlmanColorToken::Separator(separator));
                    return Ok((input, line_tokens));
                }
                _ => {}
            };

            let mut param_parser = recognize(pair(
                take_while(|ch: char| ch == '-'),
                take_while(|ch: char| ch.is_ascii_alphanumeric()),
            ));

            let (input, param) = param_parser(input)?;
            line_tokens.push(CurlmanColorToken::ParamKey(param));
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }
            *parser_state = EditorParserState::ExpectingParamValueStart;
            return Ok((input, line_tokens));
        }
        EditorParserState::ExpectingParamValueStart => {
            let (input, space) = multispace0(input)?;
            if !space.is_empty() {
                line_tokens.push(CurlmanColorToken::Whitespace(space));
            }
            let tag_parser =
                take_while1::<_, _, Error<&str>>(|ch: char| ch.is_ascii_alphanumeric());

            let param_value_res = tag_parser(input);

            return match param_value_res {
                Ok((input, param_value)) => {
                    line_tokens.push(CurlmanColorToken::ParamValue(param_value));
                    let (input, space) = multispace0(input)?;
                    if !space.is_empty() {
                        line_tokens.push(CurlmanColorToken::Whitespace(space));
                    }
                    *parser_state = EditorParserState::ExpectingParamKey;
                    Ok((input, line_tokens))
                }
                Err(_) => {
                    let string_parse_res = string_parser_global(input);
                    match string_parse_res {
                        Ok((input, string)) => {
                            line_tokens.push(CurlmanColorToken::ParamValue(string));
                            *parser_state = EditorParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => {
                            let (input, string_start) = alt((tag("\'"), tag("\"")))(input)?;
                            line_tokens.push(CurlmanColorToken::ParamValue(string_start));
                            let (input, space) = multispace0(input)?;
                            if !space.is_empty() {
                                line_tokens.push(CurlmanColorToken::Whitespace(space));
                            }
                            *parser_state = EditorParserState::ExpectingParamValueEnd(string_start);

                            Ok((input, line_tokens))
                        }
                    }
                }
            };
        }
        EditorParserState::ExpectingParamValueEnd(ref delimiter) => {
            let line_consume_res = is_not::<_, _, nom::error::Error<&str>>(*delimiter)(input);

            match line_consume_res {
                Ok((input, line)) => {
                    line_tokens.push(CurlmanColorToken::ParamValue(line));

                    let end_string_res = tag::<_, _, nom::error::Error<&str>>(*delimiter)(input);

                    match end_string_res {
                        Ok((input, end)) => {
                            line_tokens.push(CurlmanColorToken::ParamValue(end));
                            *parser_state = EditorParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => Ok((input, line_tokens)),
                    }
                }
                Err(_) => {
                    let end_string_res = tag::<_, _, nom::error::Error<&str>>(*delimiter)(input);

                    match end_string_res {
                        Ok((input, end)) => {
                            line_tokens.push(CurlmanColorToken::ParamValue(end));
                            *parser_state = EditorParserState::ExpectingParamKey;
                            Ok((input, line_tokens))
                        }
                        Err(_) => Ok((input, line_tokens)),
                    }
                }
            }
        }
    }
}

pub fn parse_curlman_editor<'a>(input: &'a Vec<String>) -> Vec<Vec<CurlmanColorToken<'a>>> {
    let mut editor_tokens = Vec::new();
    let mut line_tokens = Vec::new();
    let mut parser_state = EditorParserState::ExpectingCurl;

    for line in input {
        let mut curr_str: &str = line;
        while !curr_str.is_empty() {
            match parse_curlman_editor_line(curr_str, &mut parser_state) {
                Ok((input, new_tokens)) => {
                    line_tokens.extend(new_tokens);
                    curr_str = input;
                }
                Err(_) => {
                    line_tokens.push(CurlmanColorToken::Unknown(curr_str));
                    break;
                }
            }
        }
        if !line_tokens.is_empty() {
            editor_tokens.push(std::mem::take(&mut line_tokens));
        } else {
            editor_tokens.push(vec![CurlmanColorToken::Whitespace("")]);
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

pub fn parse_request_json(input: &'_ Vec<String>) -> Vec<Vec<JsonToken<'_>>> {
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
