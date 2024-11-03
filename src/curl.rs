use std::{collections::HashMap, io::Read, sync::atomic::AtomicU16};

use crate::{
    editor::{
        self,
        colors::{get_default_output_colorscheme, JsonOutputColorscheme},
        CurlmanWidget, InputListener, WidgetCommand,
    },
    error::Error,
    parser::{parse_json_value, JsonToken},
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
    text::Text,
    widgets::{Block, StatefulWidgetRef, Widget},
};
use serde_json::Value;

#[derive(Debug)]
enum FormatterError {
    InvalidInput,
}

pub struct RequestExecutor {
    block: Block<'static>,
    selected: bool,
    row: usize,
    top_row: u16,
    executor_height: AtomicU16,
    json_formatter: JsonFormatter,
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
            row: 0,
            top_row: 0,
            executor_height: AtomicU16::new(0),
            headers: HashMap::new(),
            handle: Easy::new(),
            json_formatter: JsonFormatter {
                colorscheme: get_default_output_colorscheme(),
            },
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

    pub fn fit_json_parser_output(&self, tokenized_lines: Vec<Vec<JsonToken>>, area: Rect) -> Text {
        let mut spans = Vec::new();
        let mut lines = Vec::new();

        for (row_idx, line) in tokenized_lines.into_iter().enumerate() {
            if row_idx < self.top_row as usize
                || row_idx >= self.top_row as usize + area.height as usize
            {
                continue;
            }

            let mut current_col = 0;
            for token in line {
                let color = token.get_color(&self.json_formatter.colorscheme);
                let mut is_cursor_set = true;
                match token {
                    JsonToken::ObjectBracket(text)
                    | JsonToken::ArrayBracket(text)
                    | JsonToken::NameSeparator(text)
                    | JsonToken::ValueSeparator(text)
                    | JsonToken::Literal(text)
                    | JsonToken::String(text)
                    | JsonToken::Whitespace(text) => {}
                }
            }
        }

        Text::from(lines)
    }

    pub fn get_executor_output(&self, area: Rect) -> Text {
        match self.headers.get(&http::header::CONTENT_TYPE) {
            Some(content_type) => match content_type.as_str() {
                _ => Text::from(String::from_utf8(self.output_data.clone()).unwrap()),
            },
            None => Text::from(String::from_utf8(self.output_data.clone()).unwrap()),
        }
    }
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
        let inner = self.get_executor_output(area);
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

struct JsonFormatter {
    pub colorscheme: JsonOutputColorscheme,
}

impl JsonFormatter {
    fn format(&self, input: &[u8]) -> Result<Vec<Vec<JsonToken>>, FormatterError> {
        let json_value: Value =
            serde_json::from_slice(input).map_err(|_| FormatterError::InvalidInput)?;

        let formatted = parse_json_value(&json_value, 0);

        Ok(formatted)
    }
}
