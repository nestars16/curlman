use crate::{editor::CurlmanWidget, keys};
use http::method::Method;
use std::{collections::HashMap, str::FromStr, time::Duration};
use url::Url;

pub struct PaneParent {
    pub layout_idx: u32,
    pub layout_pos_idx: usize,
}

#[derive(Clone)]
pub struct TargetId(pub usize);
pub struct DirectionArray(pub [Option<TargetId>; 4]);

impl DirectionArray {
    pub const NONE: Self = DirectionArray([None, None, None, None]);
}

pub struct PaneWidget {
    pub widget: Box<dyn CurlmanWidget>,
    pub layout_idx: usize,
    pub available_directions: DirectionArray,
}

impl PaneWidget {
    pub fn new(
        widget: Box<dyn CurlmanWidget>,
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

    pub fn get_next_widget_idx(
        &self,
        current_widget_idx: usize,
        direction_to_move: keys::Direction,
    ) -> Option<usize> {
        let Some(widget) = self.widgets.get(current_widget_idx) else {
            return None;
        };

        let Some(new_widget_id) = widget.available_directions.0[direction_to_move as usize].clone()
        else {
            return None;
        };

        Some(new_widget_id.0)
    }
}

#[derive(Debug, PartialEq)]
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
