use crate::{editor::CurlmanWidget, keys, App, AppState};
use http::method::Method;
use ratatui::layout;
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

#[derive(Clone)]
pub struct TargetId(pub usize);
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
    pub widgets: Vec<PaneWidget>,
}

impl Pane {
    pub fn new(widgets: Vec<PaneWidget>, parent: Option<LayoutParent>, layout_id: u32) -> Self {
        Pane {
            widgets,
            layout_parent: parent,
            layout_id,
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
    pub url: Option<Url>,
    pub method: Method,
    pub timeout: Duration,
    pub body: Option<Vec<u8>>,
    pub file_position: Option<RequestInfoFileMetadata>,
}

impl Default for RequestInfo {
    fn default() -> Self {
        Self {
            headers: HashMap::new(),
            method: Method::GET,
            url: None,
            timeout: Duration::from_secs(30),
            body: None,
            file_position: None,
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
            "X" => Ok(Self::Method),
            "H" => Ok(Self::Header),
            "d" | "data" => Ok(Self::Body(BodyType::Json)),
            _ => Err(()),
        }
    }
}
