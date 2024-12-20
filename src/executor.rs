use ratatui::{
    style::{Color, Modifier},
    text::{Line, Span},
    widgets::{Paragraph, Wrap},
};
use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    fmt::Write,
    io::{Cursor, Read},
};
use tui_textarea::{CursorMove, TextArea};

use crate::{
    editor::{
        self,
        colors::{get_default_output_colorscheme, JsonOutputColorscheme},
        get_round_bordered_box,
        widget_common::fit_and_process_text_tokens_into_editor_window,
        CurlmanWidget, CursorMovement, InputListener, VimCommand, VimMotion, WidgetCommand,
    },
    error::Error,
    keys,
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
    widgets::{Block, StatefulWidgetRef, Widget},
};

#[derive(Debug)]
enum FormatterError {
    InvalidInput,
}

enum RequestExecutorView {
    RequestBody,
    Headers,
}

pub struct RequestExecutor<'widget> {
    header_paragraph_scroll: u16,
    block: Block<'static>,
    selected: bool,
    body_cursor: Option<Cursor<Vec<u8>>>,
    json_formatter: JsonFormatter,
    error: Option<Error>,
    handle: Easy,
    output_data: Vec<u8>,
    editor: TextArea<'widget>,
    headers: HashMap<HeaderName, String>,
    request: Option<RequestInfo>,
    view: RequestExecutorView,
}

impl<'widget> RequestExecutor<'_> {
    pub fn new() -> Self {
        Self {
            header_paragraph_scroll: 0,
            view: RequestExecutorView::RequestBody,
            block: editor::get_round_bordered_box().title("Executor"),
            selected: false,
            output_data: Vec::new(),
            request: None,
            headers: HashMap::new(),
            handle: Easy::new(),
            json_formatter: JsonFormatter {
                colorscheme: get_default_output_colorscheme(),
            },
            editor: TextArea::default(),
            body_cursor: None,
            error: None,
        }
    }

    pub fn perform_curl_request(&mut self) -> Result<(), Error> {
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

        self.handle.ssl_verify_peer(false)?;
        self.handle.ssl_verify_host(false)?;

        for flag in req.flags {
            match flag {
                crate::types::CurlFlag::Insecure => {}
            }
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

    fn move_cursor_or_headers(&mut self, operator: CursorMovement) {
        match self.view {
            RequestExecutorView::RequestBody => self.move_cursor(operator),
            RequestExecutorView::Headers => match operator {
                CursorMovement::Regular(movement) => match movement {
                    CursorMove::Up => {
                        if self.header_paragraph_scroll >= 1 {
                            self.header_paragraph_scroll -= 1;
                        }
                    }
                    CursorMove::Down => {
                        self.header_paragraph_scroll += 1;
                    }
                    _ => {}
                },
                _ => {}
            },
        }
    }

    fn perform_request(&mut self) -> Option<WidgetCommand> {
        self.error = None;
        self.output_data.clear();
        let perform_request_result = self.perform_curl_request();
        match perform_request_result {
            Ok(_) => match self.headers.get(&http::header::CONTENT_TYPE) {
                Some(content_type) if content_type.as_str().contains("application/json") => {
                    self.editor = TextArea::from(
                        self.json_formatter
                            .format_output_data_into_lines(&self.output_data)
                            .unwrap_or(vec!["Json formatting failed".to_string()]),
                    )
                }
                _ => {
                    self.editor = String::from_utf8_lossy(&self.output_data)
                        .to_string()
                        .split('\n')
                        .map(|str| str.to_string())
                        .collect();
                }
            },
            Err(e) => self.error = Some(e),
        }
        None
    }

    fn move_cursor(&mut self, operator: CursorMovement) {
        if let Ok(movement) = TryInto::<CursorMove>::try_into(operator.clone()) {
            self.editor.move_cursor(movement);
            return;
        }

        let lines = self.editor.lines();
        let (row, col) = self.editor.cursor();
        match operator {
            CursorMovement::Until(char) => {
                let current_line = &lines[row][col..];
                if let Some(idx) = current_line.find(char) {
                    self.editor
                        .move_cursor(CursorMove::Jump(row as u16, idx as u16))
                }
            }
            _ => {}
        }
    }
}

impl<'widget> StatefulWidgetRef for RequestExecutor<'widget> {
    #[doc = " State associated with the stateful widget."]
    #[doc = ""]
    #[doc = " If you don\'t need this then you probably want to implement [`WidgetRef`] instead."]
    type State = AppState;

    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom stateful widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
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
            RequestExecutorView::RequestBody => {
                self.editor.render(text_area, buf);
            }
            RequestExecutorView::Headers => {
                let mut paragraph_buf = String::new();

                self.headers.iter().for_each(|(k, v)| {
                    write!(&mut paragraph_buf, "{k}: {v}")
                        .expect("Paragraph buffer string couldn't write");
                });

                Paragraph::new(paragraph_buf)
                    .wrap(Wrap { trim: true })
                    .scroll((self.header_paragraph_scroll, 0))
                    .render(text_area, buf)
            }
        }
    }
}

impl<'widget> InputListener for RequestExecutor<'widget> {
    fn handle_event(&mut self, e: crossterm::event::Event) -> Option<WidgetCommand> {
        match e {
            crossterm::event::Event::Key(key_event) => match key_event {
                KeyEvent {
                    code: KeyCode::Up, ..
                } => self.move_cursor_or_headers(CursorMovement::Regular(CursorMove::Up)),
                KeyEvent {
                    code: KeyCode::Down,
                    ..
                } => self.move_cursor_or_headers(CursorMovement::Regular(CursorMove::Down)),
                KeyEvent {
                    code: KeyCode::Left,
                    ..
                } => self.move_cursor_or_headers(CursorMovement::Regular(CursorMove::Back)),
                KeyEvent {
                    code: KeyCode::Right,
                    ..
                } => self.move_cursor_or_headers(CursorMovement::Regular(CursorMove::Forward)),
                KeyEvent {
                    code: KeyCode::Char(ch),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => return editor::widget_common::move_widget_selection(ch),
                KeyEvent {
                    code: KeyCode::Char(ch),
                    ..
                } => {
                    if let Ok(VimMotion::Move(movement)) = ch.try_into() {
                        self.move_cursor(movement);
                    }
                    match ch {
                        'E' => {
                            return self.perform_request();
                        }
                        'H' if matches!(self.view, RequestExecutorView::RequestBody) => {
                            self.view = RequestExecutorView::Headers;
                            return Some(WidgetCommand::Clear {});
                        }
                        'B' if matches!(self.view, RequestExecutorView::Headers) => {
                            self.view = RequestExecutorView::RequestBody;
                            return Some(WidgetCommand::Clear {});
                        }

                        keys::UP if matches!(self.view, RequestExecutorView::Headers) => {
                            if self.header_paragraph_scroll >= 1 {
                                self.header_paragraph_scroll -= 1;
                            }
                        }
                        keys::DOWN if matches!(self.view, RequestExecutorView::Headers) => {
                            self.header_paragraph_scroll += 1;
                        }

                        keys::UP | keys::DOWN | keys::LEFT | keys::RIGHT
                            if matches!(self.view, RequestExecutorView::RequestBody) =>
                        {
                            self.move_cursor(
                                ch.try_into()
                                    .expect("Char is guaranteed to be valid direction"),
                            )
                        }
                        _ => {}
                    };
                }
                _ => {}
            },
            _ => {}
        }
        None
    }
}

impl<'widget> CurlmanWidget for RequestExecutor<'widget> {
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
