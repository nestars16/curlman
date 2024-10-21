use crate::editor::CurlmanWidget;
use http::method::Method;
use std::{collections::HashMap, str::FromStr, time::Duration};
use url::Url;

pub struct PaneParent {
    pub layout_idx: u32,
    pub layout_pos_idx: usize,
}

const UP: u8 = 1;
const DOWN: u8 = 2;
const RIGHT: u8 = 3;
const LEFT: u8 = 4;

struct TargetId(usize);
pub struct AvailableDirections([TargetId; 4]);

impl AvailableDirections {
    pub const NONE: Self =
        AvailableDirections([TargetId(0), TargetId(0), TargetId(0), TargetId(0)]);
}

pub struct PaneWidget {
    pub widget: Box<dyn CurlmanWidget>,
    pub layout_idx: usize,
    pub available_directions: AvailableDirections,
}

impl PaneWidget {
    pub fn new(
        widget: Box<dyn CurlmanWidget>,
        layout_id: usize,
        available_directions: AvailableDirections,
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
    pub parent: Option<PaneParent>,
    pub layout_id: u32,
    pub widgets: Vec<PaneWidget>,
}

impl Pane {
    pub fn new(widgets: Vec<PaneWidget>, parent: Option<PaneParent>, layout_id: u32) -> Self {
        Pane {
            widgets,
            parent,
            layout_id,
        }
    }
}

#[derive(Debug)]
pub struct RequestInfo {
    pub headers: HashMap<String, String>,
    pub url: Option<Url>,
    pub method: Method,
    pub timeout: Duration,
    pub body: Option<Vec<u8>>,
}

impl Default for RequestInfo {
    fn default() -> Self {
        Self {
            headers: HashMap::new(),
            method: Method::GET,
            url: None,
            timeout: Duration::from_secs(30),
            body: None,
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
