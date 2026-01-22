use crate::{editor::CurlmanWidget, keys, AppState};
use http::method::Method;
use std::{collections::HashMap, str::FromStr, time::Duration};

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
    pub headers: HashMap<String, String>,
    pub url: Option<String>,
    pub method: Method,
    pub timeout: Duration,
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
            headers: HashMap::new(),
            method: Method::GET,
            url: None,
            timeout: Duration::from_secs(30),
            body: None,
            flags: vec![],
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum CurlFlagType {
    UploadFile,
    Get,
    Head,
    Insecure,
    UserAgent,
    Method,
    Header,
    DataAscii,
    Json,
    ConnectionTimeout,
    BasicAuth,
    Compression,
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
    Proto,
    Rate,
    Raw,
    Retry,
    RetryDelay,
    RetryMaxTime,
    SpeedTime,
    Url,
    UrlQuery,
    RequestTarget,
    Post301,
    Post302,
    Post303,
    MaxRedirs,
}

impl CurlFlagType {
    pub fn needs_value(&self) -> bool {
        match self {
            CurlFlagType::Insecure => false,
            CurlFlagType::Method => true,
            CurlFlagType::Header => true,
            CurlFlagType::DataAscii => true,
            CurlFlagType::Json => true,
            CurlFlagType::ConnectionTimeout => true,
            CurlFlagType::BasicAuth => false,
            CurlFlagType::Compression => false,
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
            CurlFlagType::Proto => true,
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
            CurlFlagType::UploadFile => true,
            CurlFlagType::UserAgent => true,
            CurlFlagType::Post301 => false,
            CurlFlagType::Post302 => false,
            CurlFlagType::Post303 => false,
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
    type Err = crate::error::parser::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use crate::error::parser::Error;
        match s {
            "--basic" => Ok(CurlFlagType::BasicAuth),
            "--compressed" => Ok(CurlFlagType::Compression),
            "--connect-timeout" => Ok(CurlFlagType::ConnectionTimeout),
            "-b" | "--cookie" => Ok(CurlFlagType::SetCookie),
            "-c" | "--cookie-jar" => Ok(CurlFlagType::WriteCookie),
            "--create-dirs" => Ok(CurlFlagType::CreateDirs),
            "-d" | "--data" | "--data-ascii" => Ok(Self::DataAscii),

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
            "-T" | "--upload-file" => Ok(Self::UploadFile),
            "--retry" => Ok(Self::Retry),
            "--retry-delay" => Ok(Self::RetryDelay),
            "--retry-max-time" => Ok(Self::RetryMaxTime),
            "-Y" | "--speed-limit" => Ok(Self::SpeedLimit),
            "-y" | "--speed-time" => Ok(Self::SpeedTime),
            "-A" | "--user-agent" => Ok(Self::UserAgent),
            "--url" => Ok(Self::Url),
            "--url-query" => Ok(Self::UrlQuery),
            "-u" | "--user" => Ok(Self::User),
            "--post301" => Ok(Self::Post301),
            "--post302" => Ok(Self::Post302),
            "--post303" => Ok(Self::Post303),
            _ => Err(Error::InvalidFlag(s.to_string())),
        }
    }
}
