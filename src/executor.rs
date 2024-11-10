use ratatui::{
    style::{Color, Modifier},
    text::{Line, Span},
    widgets::{Clear, Paragraph, WidgetRef, Wrap},
};
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    io::{Cursor, Read},
    sync::atomic::{AtomicU16, Ordering},
};
use tui_widget_list::{ListBuilder, ListState, ListView};

use crate::{
    editor::{
        self,
        colors::{get_default_output_colorscheme, JsonOutputColorscheme},
        get_round_bordered_box,
        widget_common::{self, fit_and_process_text_tokens_into_editor_window},
        CurlmanWidget, CursorMovement, InputListener, WidgetCommand,
    },
    error::Error,
    parser::{parse_json_editor, JsonToken},
    types::RequestInfo,
    AppState,
};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use curl::easy::{Easy, ReadError};
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
    widgets::{Block, StatefulWidget, StatefulWidgetRef, Widget},
};

#[derive(Debug)]
enum FormatterError {
    InvalidInput,
}

enum RequestExecutorView {
    RequestBody,
    Headers,
}

pub struct RequestExecutor {
    block: Block<'static>,
    selected: bool,
    body_cursor: Option<Cursor<Vec<u8>>>,
    row: usize,
    col: usize,
    top_row: u16,
    header_list_state: ListState,
    executor_height: AtomicU16,
    executor_width: AtomicU16,
    json_formatter: JsonFormatter,
    error: Option<Error>,
    handle: Easy,
    output_data: Vec<u8>,
    editor_representation: Vec<String>,
    headers: HashMap<HeaderName, String>,
    request: Option<RequestInfo>,
    view: RequestExecutorView,
}

impl RequestExecutor {
    pub fn new() -> Self {
        Self {
            view: RequestExecutorView::RequestBody,
            block: editor::get_round_bordered_box().title("Executor"),
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
            body_cursor: None,
            error: None,
            header_list_state: ListState::default(),
        }
    }

    pub fn perform_request(&mut self) -> Result<(), Error> {
        let Some(req) = self.request.clone() else {
            return Err(Error::InvalidState("No request selected"));
        };

        let Some(url) = req.url else {
            return Err(Error::MissingUrl("The Request URL is missing"));
        };

        self.headers.clear();
        self.handle.url(url.as_str())?;

        let mut header_list = curl::easy::List::new();
        for (key, value) in req.headers {
            header_list.append(&format!("{key}: {value}"))?;
        }
        self.handle.http_headers(header_list)?;
        self.handle.timeout(req.timeout)?;

        match req.method {
            Method::GET | Method::HEAD | Method::DELETE | Method::OPTIONS => {
                match req.method {
                    Method::GET => self.handle.get(true)?,
                    Method::HEAD => self.handle.custom_request("HEAD")?,
                    Method::OPTIONS => self.handle.custom_request("OPTIONS")?,
                    Method::DELETE => self.handle.custom_request("DELETE")?,
                    _ => {}
                }
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
            Method::POST | Method::PATCH | Method::PUT => {
                match req.method {
                    Method::POST => self.handle.post(true)?,
                    Method::PATCH => self.handle.custom_request("PATCH")?,
                    Method::PUT => self.handle.put(true)?,
                    _ => {}
                }

                let Some(body) = req.body else {
                    return Err(Error::NoBody);
                };

                self.body_cursor = Some(Cursor::new(body));

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

                transfer.read_function(|into| {
                    match self
                        .body_cursor
                        .as_mut()
                        .expect("Body Cursor MUST exist")
                        .read(into)
                    {
                        Ok(n) => Ok(n),
                        Err(_) => Err(ReadError::Abort),
                    }
                })?;

                transfer.write_function(|data| {
                    self.output_data.extend_from_slice(data);
                    Ok(data.len())
                })?;

                transfer.perform().map_err(|e| e.into())
            }
            _ => Err(Error::UnsupportedMethod(req.method.to_string())),
        }
    }

    fn adjust_viewport(&mut self) {
        let editor_height_val = self
            .executor_height
            .load(std::sync::atomic::Ordering::Relaxed);

        let editor_width_val = self
            .executor_width
            .load(std::sync::atomic::Ordering::Relaxed);

        if let Some(new_top_row) = widget_common::adjust_viewport(
            editor_height_val as i32,
            editor_width_val,
            self.top_row as i32,
            self.row as i32,
            &self.editor_representation,
        ) {
            self.top_row = new_top_row
        }
    }

    fn move_cursor(&mut self, operator: CursorMovement) {
        if let Some((new_row, new_col)) = widget_common::get_next_cursor_position(
            self.row,
            self.col,
            &self.editor_representation,
            operator,
        ) {
            self.col = new_col;
            self.row = new_row;
            self.adjust_viewport()
        }
    }

    fn get_raw_editor_representation(&self, area: Rect) -> Text {
        let mut is_cursor_set = false;
        let mut current_col = 0;
        let mut lines = Vec::new();
        let mut spans = Vec::new();

        for (idx, line) in self.editor_representation.iter().enumerate() {
            let cursor_has_to_be_set = self.row == idx;
            fit_and_process_text_tokens_into_editor_window(
                self.col,
                line,
                Color::White,
                area.width,
                cursor_has_to_be_set,
                &mut is_cursor_set,
                &mut current_col,
                &mut lines,
                &mut spans,
            );
        }

        Text::from(lines)
    }
}

impl StatefulWidgetRef for RequestExecutor {
    #[doc = " State associated with the stateful widget."]
    #[doc = ""]
    #[doc = " If you don\'t need this then you probably want to implement [`WidgetRef`] instead."]
    type State = AppState;

    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom stateful widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        self.executor_width.store(area.width, Ordering::Relaxed);
        self.executor_height.store(area.height, Ordering::Relaxed);
        let text_area = self.block.inner(area);
        self.block.clone().render(area, buf);

        if let Some(e) = &self.error {
            Paragraph::new(format!(
                "{}{}",
                "\n".repeat(text_area.height.div_euclid(2).checked_sub(1).unwrap_or(0) as usize),
                e.to_string()
            ))
            .wrap(Wrap { trim: false })
            .alignment(ratatui::layout::Alignment::Center)
            .style(
                Style::new()
                    .fg(Color::Red)
                    .add_modifier(Modifier::BOLD)
                    .add_modifier(Modifier::ITALIC),
            )
            .render(text_area, buf);
            return;
        }

        match self.view {
            RequestExecutorView::RequestBody => match self.editor_representation.is_empty() {
                true => {
                    Paragraph::new(String::from_utf8_lossy(&self.output_data))
                        .wrap(Wrap { trim: false })
                        .render(text_area, buf);
                }
                false => match self.headers.get(&http::header::CONTENT_TYPE) {
                    Some(content_type) => match content_type.as_str() {
                        s if s.contains("application/json") => {
                            let lines = self.json_formatter.format_lines_into_text(
                                &self.editor_representation,
                                area,
                                self.top_row as usize,
                                self.col,
                                self.row,
                                self.selected,
                            );
                            (Text::from(lines)).render(text_area, buf);
                        }
                        _ => {
                            self.get_raw_editor_representation(area)
                                .render(text_area, buf);
                        }
                    },
                    None => {
                        self.get_raw_editor_representation(area)
                            .render(text_area, buf);
                    }
                },
            },
            RequestExecutorView::Headers => {
                let request_headers = self
                    .headers
                    .iter()
                    .map(|(k, v)| {
                        let space = 1 + v.len().div_ceil(area.width as usize);
                        (
                            Paragraph::new(vec![Line::from_iter(
                                [
                                    Span::raw(k.as_str().to_string()).bold(),
                                    Span::raw(": ".to_string()).bold(),
                                    Span::raw(v.to_string()).italic(),
                                ]
                                .into_iter(),
                            )])
                            .wrap(Wrap { trim: true })
                            .left_aligned(),
                            if space > 2 { space } else { 1 },
                        )
                    })
                    .collect::<Vec<_>>();

                let header_count = request_headers.len();

                let builder = ListBuilder::new(move |context| {
                    let (mut paragraph, height) = request_headers[context.index].clone();

                    if context.is_selected {
                        paragraph = paragraph.reversed();
                    }

                    (paragraph, height as u16)
                });

                let list = ListView::new(builder, header_count);
                list.render(text_area, buf, &mut state.header_list_state);
            }
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
                    code: KeyCode::Up, ..
                } => self.move_cursor(CursorMovement::Up),
                KeyEvent {
                    code: KeyCode::Down,
                    ..
                } => self.move_cursor(CursorMovement::Down),
                KeyEvent {
                    code: KeyCode::Left,
                    ..
                } => self.move_cursor(CursorMovement::Left),
                KeyEvent {
                    code: KeyCode::Right,
                    ..
                } => self.move_cursor(CursorMovement::Right),
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
                        self.error = None;
                        self.output_data.clear();
                        let perform_request_result = self.perform_request();
                        self.col = 0;
                        self.row = 0;
                        self.top_row = 0;
                        match perform_request_result {
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
                                    _ => {
                                        self.editor_representation =
                                            String::from_utf8_lossy(&self.output_data)
                                                .to_string()
                                                .split('\n')
                                                .map(|str| str.to_string())
                                                .collect();
                                    }
                                },
                                None => {
                                    self.output_data =
                                        "The response contained no Content-Type header".into();
                                }
                            },
                            Err(e) => self.error = Some(e),
                        }

                        return None;
                    };
                    match self.view {
                        RequestExecutorView::RequestBody => match ch {
                            'H' => {
                                self.view = RequestExecutorView::Headers;
                                return Some(WidgetCommand::Clear {
                                    is_header_map_empty: self.headers.is_empty(),
                                });
                            }
                            _ => {}
                        },
                        RequestExecutorView::Headers => {
                            match ch {
                                'B' => {
                                    self.view = RequestExecutorView::RequestBody;

                                    return Some(WidgetCommand::Clear {
                                        is_header_map_empty: self.headers.is_empty(),
                                    });
                                }
                                _ => {}
                            };
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }

        None
    }
}

impl CurlmanWidget for RequestExecutor {
    fn toggle_selected(&mut self) {
        self.selected = !self.selected;
        if self.selected {
            self.block = get_round_bordered_box()
                .border_style(Style::new().red())
                .title("Executor");
        } else {
            self.block = get_round_bordered_box().title("Executor");
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
        let jv_output_string = unsafe { CStr::from_ptr(jv_c_string_val) }
            .to_string_lossy()
            .to_string();

        unsafe { jv_free(dump_string) };

        Ok(jv_output_string.split('\n').map(|l| l.to_owned()).collect())
    }

    pub fn format_lines_into_text<'a>(
        &self,
        editor_lines: &'a Vec<String>,
        area: Rect,
        top_row: usize,
        editor_col: usize,
        editor_row: usize,
        selected: bool,
    ) -> Text<'a> {
        let tokenized_lines = parse_json_editor(editor_lines);
        let mut lines = Vec::new();
        let mut spans = Vec::new();
        let mut is_cursor_set = false;

        for (idx, line) in tokenized_lines.iter().enumerate() {
            if idx < top_row as usize || idx >= top_row as usize + area.height as usize {
                continue;
            }

            let mut current_col = 0;

            for token in line {
                let color = token.get_color(&self.colorscheme);
                let line_token_len = token.get_str().len();
                let line_token_end = (current_col + line_token_len).checked_sub(1).unwrap_or(0);

                let cursor_has_to_be_set =
                    selected && !is_cursor_set && idx == editor_row && line_token_end >= editor_col;

                match token {
                    JsonToken::ObjectBracket(text)
                    | JsonToken::ArrayBracket(text)
                    | JsonToken::KeySeparator(text)
                    | JsonToken::Identifier(text)
                    | JsonToken::ValueSeparator(text)
                    | JsonToken::Literal(text)
                    | JsonToken::String(text)
                    | JsonToken::Whitespace(text)
                    | JsonToken::Invalid(text) => {
                        fit_and_process_text_tokens_into_editor_window(
                            editor_col,
                            text,
                            color,
                            area.width - 2,
                            cursor_has_to_be_set,
                            &mut is_cursor_set,
                            &mut current_col,
                            &mut lines,
                            &mut spans,
                        );
                    }
                }

                current_col += line_token_len;
            }

            if idx == editor_row && editor_col == editor_lines[editor_row].len() {
                if editor_col as u16 == area.width {
                    lines.push(Line::from(std::mem::take(&mut spans)));
                }

                spans.push(Span::raw(" ").reversed());
            }

            lines.push(Line::from(std::mem::take(&mut spans)));
        }

        if !spans.is_empty() {
            lines.push(Line::from(std::mem::take(&mut spans)));
        }

        Text::from(lines)
    }
}

mod tests {
    use super::JsonFormatter;
    use crate::editor::colors::get_default_output_colorscheme;
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
}
