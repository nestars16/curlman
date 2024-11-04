use ratatui::widgets::WidgetRef;
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    io::Read,
    sync::atomic::{AtomicU16, Ordering},
};

use crate::{
    editor::{
        self,
        colors::{get_default_output_colorscheme, JsonOutputColorscheme},
        widget_common::fit_tokens_into_editor_window,
        CurlmanWidget, InputListener, WidgetCommand,
    },
    error::Error,
    parser::{parse_json_editor, JsonToken},
    types::RequestInfo,
    AppState,
};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use curl::easy::{Easy, List, ReadError};
use http::{HeaderName, Method};
use jq_sys::{
    jv_copy, jv_dump_string, jv_free, jv_get_kind, jv_invalid_get_msg, jv_invalid_has_msg,
    jv_kind_JV_KIND_INVALID, jv_parse, jv_print_flags_JV_PRINT_PRETTY,
    jv_print_flags_JV_PRINT_SPACE1, jv_string_value,
};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Style, Stylize},
    text::Text,
    widgets::{Block, StatefulWidgetRef, Widget},
};

#[derive(Debug)]
enum FormatterError {
    InvalidInput,
}

pub struct RequestExecutor {
    block: Block<'static>,
    selected: bool,
    row: usize,
    col: usize,
    top_row: u16,
    executor_height: AtomicU16,
    executor_width: AtomicU16,
    json_formatter: JsonFormatter,
    handle: Easy,
    output_data: Vec<u8>,
    editor_representation: Vec<String>,
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
            editor_representation: Vec::new(),
            executor_width: AtomicU16::new(0),
            col: 0,
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

    pub fn get_executor_output(&self, area: Rect) -> Text {
        Text::from("")
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
        self.executor_width.store(area.width, Ordering::Relaxed);
        self.executor_height.store(area.height, Ordering::Relaxed);
        let text_area = self.block.inner(area);
        self.block.clone().render(area, buf);

        match self.editor_representation.is_empty() {
            true => {
                Text::from(String::from_utf8_lossy(&self.output_data)).render(text_area, buf);
            }
            false => match self.headers.get(&http::header::CONTENT_TYPE) {
                Some(content_type) => match content_type.as_str() {
                    "application/json" => {
                        Text::from(self.json_formatter.format_lines_into_text(
                            &self.editor_representation,
                            area.width,
                            self.col,
                        ))
                        .render(text_area, buf);
                    }
                    _ => {
                        let editor = self.editor_representation.join("\n");
                        Text::from(editor).render(text_area, buf);
                    }
                },
                None => {
                    let editor = self.editor_representation.join("\n");
                    Text::from(editor).render(text_area, buf);
                }
            },
        }
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
                            Ok(_) => match self.headers.get(&http::header::CONTENT_TYPE) {
                                Some(content_type) => match content_type.as_str() {
                                    s if s.contains("application/json") => {
                                        let json_lines_res = self
                                            .json_formatter
                                            .format_output_data_into_lines(&self.output_data);

                                        match json_lines_res {
                                            Ok(lines) => {
                                                self.editor_representation = lines;
                                            }
                                            Err(e) => {
                                                self.output_data =
                                                    format!("An error ocurred : {:?}", e).into()
                                            }
                                        }
                                    }
                                    _ => {}
                                },
                                None => {}
                            },
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
    fn format_output_data_into_lines(&self, input: &[u8]) -> Result<Vec<String>, FormatterError> {
        let input_str = CString::new(input).map_err(|_| FormatterError::InvalidInput)?;
        let input_jv = unsafe { jv_parse(input_str.as_ptr()) };
        let jv_kind = unsafe { jv_get_kind(input_jv) };

        if jv_kind == jv_kind_JV_KIND_INVALID {
            if unsafe { jv_invalid_has_msg(jv_copy(input_jv)) == 1 } {
                let error = unsafe { jv_invalid_get_msg(input_jv) };
                let jv_c_string_val = unsafe { jv_string_value(error) };
                let jv_error_string = unsafe { CStr::from_ptr(jv_c_string_val) }.to_string_lossy();
                unsafe { jv_free(error) }
            } else {
                unsafe { jv_free(input_jv) }
            }

            return Err(FormatterError::InvalidInput);
        }

        let dump_string = unsafe {
            jv_dump_string(
                input_jv,
                (jv_print_flags_JV_PRINT_PRETTY | jv_print_flags_JV_PRINT_SPACE1) as i32,
            )
        };
        let jv_c_string_val = unsafe { jv_string_value(dump_string) };
        let jv_output_string = unsafe { CStr::from_ptr(jv_c_string_val) }.to_string_lossy();
        unsafe { jv_free(dump_string) };
        Ok(jv_output_string.split('\n').map(|l| l.to_owned()).collect())
    }

    pub fn format_lines_into_text<'a>(
        &self,
        lines: &'a Vec<String>,
        area_width: u16,
        editor_col: usize,
    ) -> Text<'a> {
        let tokenized_lines = parse_json_editor(lines);
        panic!("{:?}", tokenized_lines);

        let mut lines = Vec::new();
        let mut spans = Vec::new();

        let mut is_cursor_set = true;
        for line in tokenized_lines {
            let mut current_col = 0;
            for token in line {
                let color = token.get_color(&self.colorscheme);
                match token {
                    JsonToken::ObjectBracket(text)
                    | JsonToken::ArrayBracket(text)
                    | JsonToken::KeySeparator(text)
                    | JsonToken::Identifier(text)
                    | JsonToken::ValueSeparator(text)
                    | JsonToken::Literal(text)
                    | JsonToken::String(text)
                    | JsonToken::Whitespace(text)
                    | JsonToken::Invalid(text) => fit_tokens_into_editor_window(
                        editor_col,
                        text,
                        color,
                        area_width,
                        false,
                        &mut is_cursor_set,
                        &mut current_col,
                        &mut lines,
                        &mut spans,
                    ),
                }
            }
        }

        Text::from(lines)
    }
}

mod tests {
    use super::JsonFormatter;
    use crate::{editor::colors::get_default_output_colorscheme, parser::parse_json_editor};
    use jq_sys::{jq_init, jq_teardown};

    #[test]
    fn test_formatting_output_data_into_lines() {
        let mut jq_state = unsafe { jq_init() };
        if jq_state.is_null() {
            panic!("Couldn't initialize jq");
        }
        let formatter = JsonFormatter {
            colorscheme: get_default_output_colorscheme(),
        };
        let input = "{\"name\" : \"John\", \"age\": 30}";
        let result = formatter.format_output_data_into_lines(input.as_bytes());

        let Ok(lines) = result else {
            panic!("Formatting output failed");
        };

        unsafe { jq_teardown(&mut jq_state) };

        assert_eq!(
            lines,
            vec![
                "{".to_string(),
                "  \"name\": \"John\",".to_string(),
                "  \"age\": 30".to_string(),
                "}".to_string(),
            ]
        );
    }

    #[test]
    fn test_formatting_lines_into_text() {
        let mut jq_state = unsafe { jq_init() };

        if jq_state.is_null() {
            panic!("Couldn't initialize jq");
        }

        let formatter = JsonFormatter {
            colorscheme: get_default_output_colorscheme(),
        };

        let input = vec![
            "{".to_string(),
            "  \"name\": \"John\",".to_string(),
            "  \"age\": 30".to_string(),
            "}".to_string(),
        ];

        let result = parse_json_editor(&input);

        dbg!(result);
        unsafe { jq_teardown(&mut jq_state) };
    }
}
