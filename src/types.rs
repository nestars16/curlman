use http::Method;
use std::{collections::HashMap, str::FromStr, time::Duration};
use url::Url;

#[derive(Debug)]
pub struct RequestInfo {
    pub headers: HashMap<String, String>,
    pub url: Option<Url>,
    pub method: Method,
    pub timeout: Duration,
}

impl Default for RequestInfo {
    fn default() -> Self {
        Self {
            headers: HashMap::new(),
            method: Method::GET,
            url: None,
            timeout: Duration::from_secs(30),
        }
    }
}

pub enum BodyType {
    Json,
}

pub enum CurlmanRequestParamType {
    Method,
    Header,
    Body(BodyType),
}

impl FromStr for CurlmanRequestParamType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-X" => Ok(Self::Method),
            "-H" => Ok(Self::Header),
            "-d" | "--data" => Ok(Self::Body(BodyType::Json)),
            _ => Err(()),
        }
    }
}
