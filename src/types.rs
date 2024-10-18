use curl::easy::HttpVersion;
use http::{header::HeaderMap, method::Method};
use std::{collections::HashMap, time::Duration};
use url::Url;

struct Request {
    url: Url,
    method: Method,
    headers: HeaderMap,
    body: Option<Vec<u8>>,
    timeout: Option<Duration>,
    version: HttpVersion,
    cookies: Option<HashMap<String, String>>,
}
