use std::{collections::HashMap, io::Read, sync::atomic::AtomicU16};

use crate::{
    editor::{self, CurlmanWidget, InputListener, WidgetCommand},
    error::Error,
    keys,
    types::RequestInfo,
    AppState,
};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use curl::easy::{Easy, List, ReadError};
use http::{HeaderName, Method};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Style, Stylize},
    text::{Line, Text},
    widgets::{Block, StatefulWidgetRef, Widget},
};

trait RequestOutputFormatter {
    fn format(&mut self, input: &[u8]) -> Vec<Line>;
}

pub struct RequestExecutor {
    block: Block<'static>,
    selected: bool,
    col: usize,
    row: usize,
    top_col: AtomicU16,
    formatter: Option<Box<dyn RequestOutputFormatter>>,
    handle: Easy,
    output_data: Vec<u8>,
    headers: HashMap<HeaderName, String>,
    request: Option<RequestInfo>,
}

impl RequestExecutor {
    pub fn new() -> Self {
        Self {
            block: editor::get_round_bordered_box(),
            selected: false,
            output_data: Vec::new(),
            request: None,
            col: 0,
            row: 0,
            top_col: AtomicU16::new(0),
            headers: HashMap::new(),
            handle: Easy::new(),
            formatter: None,
        }
    }

    pub fn perform_request(&mut self) -> Result<(), Error> {
        let Some(req) = self.request.clone() else {
            return Err(Error::InvalidState);
        };

        let Some(url) = req.url else {
            return Err(Error::InvalidUrl);
        };

        self.headers.clear();
        self.handle.url(url.as_ref())?;
        let mut header_list = List::new();
        for (key, value) in req.headers {
            header_list.append(&format!("{key}: {value}"))?;
        }
        self.handle.http_headers(header_list)?;
        self.handle.timeout(req.timeout)?;

        match req.method {
            Method::GET => {
                self.handle.get(true)?;
                let mut transfer = self.handle.transfer();

                transfer.header_function(|into| {
                    match String::from_utf8_lossy(into).split_once(':') {
                        Some((header_name, header_value)) => 'some_block: {
                            let header_name_parse_res = header_name.parse::<HeaderName>();
                            let Ok(header_name) = header_name_parse_res else {
                                break 'some_block;
                            };
                            self.headers.insert(header_name, header_value.to_string());
                            eprintln!("{:?}", self.headers);
                        }
                        None => {}
                    };

                    true
                })?;

                transfer.write_function(|data| {
                    self.output_data.extend_from_slice(data);
                    Ok(data.len())
                })?;

                transfer.perform().map_err(|e| e.into())
            }
            Method::POST => {
                self.handle.post(true)?;
                let Some(body) = req.body else {
                    return Err(Error::NoBody);
                };
                let mut transfer = self.handle.transfer();
                transfer.header_function(|into| {
                    match String::from_utf8_lossy(into).split_once(':') {
                        Some((header_name, header_value)) => 'some_block: {
                            let header_name_parse_res = header_name.parse::<HeaderName>();

                            let Ok(header_name) = header_name_parse_res else {
                                break 'some_block;
                            };
                            self.headers.insert(header_name, header_value.to_string());
                        }
                        None => {}
                    };
                    true
                })?;

                transfer.read_function(|into| match body.as_slice().read(into) {
                    Ok(read) => Ok(read),
                    Err(_) => Err(ReadError::Abort),
                })?;

                transfer.write_function(|data| {
                    self.output_data.extend_from_slice(data);
                    Ok(data.len())
                })?;

                transfer.perform().map_err(|e| e.into())
            }
            _ => {
                unimplemented!()
            }
        }
    }

    pub fn get_formatted_output() {}
}

impl StatefulWidgetRef for RequestExecutor {
    #[doc = " State associated with the stateful widget."]
    #[doc = ""]
    #[doc = " If you don\'t need this then you probably want to implement [`WidgetRef`] instead."]
    type State = AppState;

    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom stateful widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer, _: &mut Self::State) {
        let text_area = self.block.inner(area);

        let inner = Text::from(String::from_utf8(self.output_data.clone()).unwrap());

        self.block.clone().render(area, buf);

        inner.render(text_area, buf);
    }
}

impl InputListener for RequestExecutor {
    fn handle_event(&mut self, e: crossterm::event::Event) -> Option<WidgetCommand> {
        match e {
            crossterm::event::Event::FocusGained => {}
            crossterm::event::Event::FocusLost => {}
            crossterm::event::Event::Key(key_event) => match key_event {
                KeyEvent {
                    code: KeyCode::Char(ch),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => return editor::widget_common::move_widget_selection(ch),
                KeyEvent {
                    code: KeyCode::Char(ch),
                    ..
                } => {
                    if ch == 'E' {
                        self.output_data.clear();
                        match self.perform_request() {
                            Ok(_) => {}
                            Err(e) => {
                                self.output_data = format!("There was an error\n{:?}", e).into()
                            }
                        }
                    }
                }
                _ => {}
            },
            crossterm::event::Event::Mouse(_) => {}
            crossterm::event::Event::Paste(_) => {}
            crossterm::event::Event::Resize(_, _) => {}
        }

        None
    }
}

impl CurlmanWidget for RequestExecutor {
    fn toggle_selected(&mut self) {
        self.selected = !self.selected;
        if self.selected {
            self.block = Block::bordered().border_style(Style::new().red());
        } else {
            self.block = Block::bordered();
        }
    }

    fn update_shared_state(&mut self, new_state: &AppState) -> Result<(), Error> {
        if let Some(idx) = new_state.selected_request_idx {
            self.request = new_state.requests.iter().nth(idx).cloned();
        }
        Ok(())
    }
}
