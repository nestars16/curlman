use crate::{editor::CurlmanWidget, keys, AppState};
use curl::Version;
use http::method::{self, Method};
use nom_locate::LocatedSpan;
use std::{collections::HashMap, str::FromStr, time::Duration};
use url::Url;

pub struct LayoutParent {
    pub layout_idx: u32,
    pub layout_pos_idx: usize,
}

impl LayoutParent {
    pub fn new(layout_idx: u32, layout_pos_idx: usize) -> Self {
        Self {
            layout_pos_idx,
            layout_idx,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TargetId(pub usize);

#[derive(Clone, Debug)]
pub struct DirectionArray(pub [Option<TargetId>; 4]);

impl DirectionArray {
    pub const NONE: Self = DirectionArray([None, None, None, None]);
}

pub struct PaneWidget {
    pub widget: Box<dyn CurlmanWidget<State = AppState>>,
    pub layout_idx: usize,
    pub available_directions: DirectionArray,
}

impl PaneWidget {
    pub fn new(
        widget: Box<dyn CurlmanWidget<State = AppState>>,
        layout_id: usize,
        available_directions: DirectionArray,
    ) -> Self {
        PaneWidget {
            widget,
            layout_idx: layout_id,
            available_directions,
        }
    }
}

// A pane has a layout and an id, meaning that ever
pub struct Pane {
    pub layout_parent: Option<LayoutParent>,
    pub layout_id: u32,
    pub available_directions: DirectionArray,
    pub widgets: Vec<PaneWidget>,
}

impl Pane {
    pub fn new(
        widgets: Vec<PaneWidget>,
        parent: Option<LayoutParent>,
        layout_id: u32,
        available_directions: DirectionArray,
    ) -> Self {
        Pane {
            widgets,
            layout_parent: parent,
            layout_id,
            available_directions,
        }
    }

    pub fn get_next_widget_idx(
        &self,
        current_widget_idx: usize,
        direction_to_move: keys::Direction,
    ) -> Option<usize> {
        let widget = self.widgets.get(current_widget_idx)?;

        let new_widget_id = widget.available_directions.0[direction_to_move as usize].clone()?;

        Some(new_widget_id.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RequestInfoFileMetadata {
    pub start_index: usize,
    pub end_index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RequestInfo {
    pub headers: Vec<String>,
    pub url: Option<String>,
    pub method: Method,
    pub body: Option<Vec<u8>>,
    pub flags: Vec<CurlFlag>,
}

impl RequestInfo {
    pub fn default_with_url(url: String) -> Self {
        Self {
            url: Some(url),
            ..Default::default()
        }
    }
}

impl Default for RequestInfo {
    fn default() -> Self {
        Self {
            headers: Vec::new(),
            method: Method::GET,
            url: None,
            body: None,
            flags: vec![],
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum CurlFlagType {
    Get,
    Head,
    Insecure,
    UserAgent,
    Method,
    Header,
    Data,
    Json,
    ConnectionTimeout,
    BasicAuth,
    SetCookie,
    WriteCookie,
    CreateDirs,
    DataBinary,
    DataRaw,
    DataUrlEncode,
    Referer,
    Digest,
    User,
    Expect100Timeout,
    Follow,
    Form,
    FormEscape,
    FormString,
    Http09,
    Http1,
    Http11,
    Http2,
    Http2PriorKnowledge,
    Http3,
    KeepAliveCount,
    KeepAliveTime,
    Interface,
    LimitRate,
    SpeedLimit,
    Location,
    LocationTrusted,
    MaxTime,
    NoBuffer,
    Ntlm,
    Oauth2Bearer,
    Output,
    OutputDir,
    Rate,
    Raw,
    Retry,
    RetryDelay,
    RetryMaxTime,
    SpeedTime,
    Url,
    UrlQuery,
    RequestTarget,
    MaxRedirs,
}

impl CurlFlagType {
    pub fn needs_value(&self) -> bool {
        match self {
            CurlFlagType::Insecure => false,
            CurlFlagType::Method => true,
            CurlFlagType::Header => true,
            CurlFlagType::Data => true,
            CurlFlagType::Json => true,
            CurlFlagType::ConnectionTimeout => true,
            CurlFlagType::BasicAuth => false,
            CurlFlagType::SetCookie => true,
            CurlFlagType::WriteCookie => true,
            CurlFlagType::CreateDirs => false,
            CurlFlagType::DataBinary => true,
            CurlFlagType::DataRaw => true,
            CurlFlagType::DataUrlEncode => true,
            CurlFlagType::Digest => false,
            CurlFlagType::User => true,
            CurlFlagType::Expect100Timeout => true,
            CurlFlagType::Follow => false,
            CurlFlagType::Form => true,
            CurlFlagType::FormEscape => true,
            CurlFlagType::FormString => true,
            CurlFlagType::Head => false,
            CurlFlagType::Http09 => false,
            CurlFlagType::Http1 => false,
            CurlFlagType::Http11 => false,
            CurlFlagType::Http2 => false,
            CurlFlagType::Http2PriorKnowledge => false,
            CurlFlagType::Http3 => false,
            CurlFlagType::KeepAliveCount => true,
            CurlFlagType::KeepAliveTime => true,
            CurlFlagType::Interface => true,
            CurlFlagType::LimitRate => true,
            CurlFlagType::SpeedLimit => true,
            CurlFlagType::Location => false,
            CurlFlagType::LocationTrusted => false,
            CurlFlagType::MaxTime => true,
            CurlFlagType::NoBuffer => false,
            CurlFlagType::Ntlm => false,
            CurlFlagType::Oauth2Bearer => true,
            CurlFlagType::Output => true,
            CurlFlagType::OutputDir => true,
            CurlFlagType::Rate => true,
            CurlFlagType::Raw => false,
            CurlFlagType::Retry => true,
            CurlFlagType::RetryDelay => true,
            CurlFlagType::RetryMaxTime => true,
            CurlFlagType::SpeedTime => true,
            CurlFlagType::Url => true,
            CurlFlagType::UrlQuery => true,
            CurlFlagType::RequestTarget => true,
            CurlFlagType::Referer => true,
            CurlFlagType::UserAgent => true,
            CurlFlagType::MaxRedirs => true,
            CurlFlagType::Get => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct CurlFlag {
    pub flag_type: CurlFlagType,
    pub value: Option<String>,
}

impl CurlFlag {
    pub fn new(flag_type: CurlFlagType, value: Option<String>) -> Self {
        Self { flag_type, value }
    }
}

impl FromStr for CurlFlagType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "--basic" => Ok(CurlFlagType::BasicAuth),
            "--connect-timeout" => Ok(CurlFlagType::ConnectionTimeout),
            "-b" | "--cookie" => Ok(CurlFlagType::SetCookie),
            "-c" | "--cookie-jar" => Ok(CurlFlagType::WriteCookie),
            "--create-dirs" => Ok(CurlFlagType::CreateDirs),
            "-d" | "--data" | "--data-ascii" => Ok(Self::Data),

            "--data-binary" => Ok(Self::DataBinary),
            "--max-redirs" => Ok(Self::MaxRedirs),
            "--data-raw" => Ok(Self::DataRaw),
            "--data-urlencode" => Ok(Self::DataUrlEncode),
            "--digest" => Ok(Self::Digest),
            "--expect100-timeout" => Ok(Self::Expect100Timeout),
            "--follow" => Ok(Self::Follow),
            "-F" | "--form" => Ok(Self::Form),
            "--form-escape" => Ok(Self::FormEscape),
            "--form-string" => Ok(Self::FormString),

            "-H" | "--header" => Ok(Self::Header),
            "--http0.9" => Ok(Self::Http09),
            "-0" | "--http1.0" => Ok(Self::Http1),
            "--http1.1" => Ok(Self::Http11),
            "--http2" => Ok(Self::Http2),
            "--http2-prior-knowledge" => Ok(Self::Http2PriorKnowledge),
            "--http3" => Ok(Self::Http3),
            "--insecure" | "-k" => Ok(CurlFlagType::Insecure),
            "--interface" => Ok(Self::Interface),
            "--json" => Ok(Self::Json),
            "--keepalive-cnt" => Ok(Self::KeepAliveCount),
            "--keepalive-time" => Ok(Self::KeepAliveTime),
            "--limit-rate" => Ok(Self::LimitRate),
            "-L" | "--location" => Ok(Self::Location),
            "--location-trusted" => Ok(Self::LocationTrusted),
            "-m" | "--max-time" => Ok(Self::MaxTime),
            "-N" | "--no-buffer" => Ok(Self::NoBuffer),
            "--ntlm" => Ok(Self::Ntlm),
            "--oauth2-bearer" => Ok(Self::Oauth2Bearer),
            "-o" | "--output" => Ok(Self::Output),
            "--output-dir" => Ok(Self::OutputDir),
            "--rate" => Ok(Self::Rate),
            "--raw" => Ok(Self::Raw),
            "-e" | "--referer" => Ok(Self::Referer),
            "-X" | "--request" => Ok(Self::Method),
            "--request-target" => Ok(Self::RequestTarget),
            "--retry" => Ok(Self::Retry),
            "--retry-delay" => Ok(Self::RetryDelay),
            "--retry-max-time" => Ok(Self::RetryMaxTime),
            "-Y" | "--speed-limit" => Ok(Self::SpeedLimit),
            "-y" | "--speed-time" => Ok(Self::SpeedTime),
            "-A" | "--user-agent" => Ok(Self::UserAgent),
            "--url" => Ok(Self::Url),
            "--url-query" => Ok(Self::UrlQuery),
            "-u" | "--user" => Ok(Self::User),
            _ => Err(s.to_string()),
        }
    }
}

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct CurlmanToken<'a> {
    pub token_type: CurlmanTokenType,
    pub lexeme: &'a str,
    pub span: Span<'a>,
}

#[derive(Debug, PartialEq)]
pub enum CurlmanIr {
    Flag {
        flag_token_idx: usize,
        value_token_idx: Option<usize>,
        value: CurlFlag,
    },
    Url {
        token_idx: usize,
        value: Url,
    },
}

impl CurlmanIr {
    pub fn modify_request(self, req: &mut RequestInfo) {
        match self {
            Self::Url { token_idx, value } => req.url = Some(value.to_string()),
            CurlmanIr::Flag {
                flag_token_idx,
                value_token_idx,
                value: flag,
            } => match flag.flag_type {
                CurlFlagType::Insecure
                | CurlFlagType::ConnectionTimeout
                | CurlFlagType::BasicAuth
                | CurlFlagType::WriteCookie
                | CurlFlagType::CreateDirs
                | CurlFlagType::Digest
                | CurlFlagType::User
                | CurlFlagType::Expect100Timeout
                | CurlFlagType::Follow
                | CurlFlagType::Http09
                | CurlFlagType::Http1
                | CurlFlagType::Http11
                | CurlFlagType::Http2
                | CurlFlagType::Http2PriorKnowledge
                | CurlFlagType::Http3
                | CurlFlagType::KeepAliveCount
                | CurlFlagType::KeepAliveTime
                | CurlFlagType::Interface
                | CurlFlagType::LimitRate
                | CurlFlagType::SpeedLimit
                | CurlFlagType::Location
                | CurlFlagType::LocationTrusted
                | CurlFlagType::MaxTime
                | CurlFlagType::NoBuffer
                | CurlFlagType::Ntlm
                | CurlFlagType::Output
                | CurlFlagType::OutputDir
                | CurlFlagType::Rate
                | CurlFlagType::Raw
                | CurlFlagType::Retry
                | CurlFlagType::RetryDelay
                | CurlFlagType::RetryMaxTime
                | CurlFlagType::SpeedTime
                | CurlFlagType::UrlQuery
                | CurlFlagType::RequestTarget
                | CurlFlagType::MaxRedirs => {
                    req.flags.push(flag);
                }
                CurlFlagType::Get => {
                    req.method = Method::GET;
                }
                CurlFlagType::Head => {
                    req.method = Method::HEAD;
                }
                CurlFlagType::UserAgent => {
                    if let Some(value) = flag.value {
                        req.headers.push(format!("User-Agent: {value}"))
                    }
                }
                CurlFlagType::Method => {
                    let method_string = flag.value.unwrap_or("GET".to_string());
                    req.method = Method::from_str(&method_string).unwrap_or(Method::GET);
                }
                CurlFlagType::Header => {
                    if let Some(header) = flag.value {
                        req.headers.push(header)
                    }
                }
                //TODO add interpretation of special characters and body appending
                CurlFlagType::Data => req.body = Some(flag.value.unwrap_or_default().into()),
                CurlFlagType::DataBinary => req.body = Some(flag.value.unwrap_or_default().into()),
                CurlFlagType::DataRaw => req.body = Some(flag.value.unwrap_or_default().into()),
                CurlFlagType::DataUrlEncode => {
                    req.body = Some(flag.value.unwrap_or_default().into())
                }
                CurlFlagType::Json => {
                    req.headers
                        .push("Content-Type: application/json".to_string());
                    req.headers.push("Accept: application/json".to_string());
                    req.body = Some(flag.value.unwrap_or_default().into())
                }
                CurlFlagType::SetCookie => {
                    if let Some(value) = flag.value {
                        req.headers.push(format!("Cookie: {value}"));
                    }
                }
                CurlFlagType::Referer => {
                    if let Some(value) = flag.value {
                        req.headers.push(format!("Referer: {value}"))
                    }
                }
                CurlFlagType::Form => {}
                CurlFlagType::FormEscape => {}
                CurlFlagType::FormString => {}
                CurlFlagType::Oauth2Bearer => {
                    if let Some(value) = flag.value {
                        req.headers.push(format!("Authorization: Bearer {value}"))
                    }
                }
                CurlFlagType::Url => {
                    if let Some(value) = flag.value {
                        req.url = Some(value)
                    }
                }
            },
        }
    }
}

impl<'a> CurlmanToken<'a> {
    pub fn from_span(input: Span<'a>, token_type: CurlmanTokenType) -> Self {
        let s: &str = *input.fragment();

        Self {
            token_type,
            lexeme: s,
            span: input,
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

#[derive(Debug)]
pub enum EditorParserState<'a> {
    ExpectingCurl,
    ExpectingUrl,
    ExpectingParamKey,
    ExpectingParamValueStart,
    ExpectingParamValueEnd(&'a str),
}
