use ratatui::{
    style::{Color, Modifier},
    text::{Line, Span},
    widgets::{Paragraph, Wrap},
};

use std::{
    borrow::Cow,
    collections::HashMap,
    ffi::{CStr, CString},
    fmt::Write,
    io::{Cursor, Read},
};

use crate::{
    cursor_movements::{CursorMoveDirection, CursorMovement},
    editor::{
        self,
        colors::{get_default_output_colorscheme, JsonOutputColorscheme},
        get_round_bordered_box,
        widget_common::{fit_tokens_into_editor, EditorInner},
        CurlmanWidget, InputListener, WidgetCommand,
    },
    error::Error,
    keys,
    parser::{parse_request_json, JsonToken},
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

pub struct Enviroment {
    vars: HashMap<String, String>,
}

pub struct RequestExecutor {
    header_paragraph_scroll: u16,
    block: Block<'static>,
    selected: bool,
    body_cursor: Option<Cursor<Vec<u8>>>,
    error: Option<Error>,
    handle: Easy,
    output_data: Vec<u8>,
    editor: EditorInner,
    headers: HashMap<HeaderName, String>,
    request: Option<RequestInfo>,
    view: RequestExecutorView,
    env: Enviroment,
}

impl RequestExecutor {
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
            editor: EditorInner::new(),
            body_cursor: None,
            error: None,
            env: Enviroment {
                vars: HashMap::new(),
            },
        }
    }

    pub fn populate_request_url<'a>(
        &self,
        template_string: &'a str,
    ) -> Result<Cow<'a, str>, Error> {
        enum UrlTemplateParser {
            Looking,
            Open,
        }

        let mut state = UrlTemplateParser::Looking;
        let mut populated_string = String::new();
        let mut template_var_start = 0;
        let mut non_template_slice_start = 0;

        for (idx, ch) in template_string.char_indices() {
            match state {
                UrlTemplateParser::Looking if ch == '{' => {
                    let to_consume = &template_string[non_template_slice_start..idx];
                    populated_string.push_str(to_consume);
                    template_var_start = idx + 1;
                    state = UrlTemplateParser::Open;
                }
                UrlTemplateParser::Open if ch == '}' => {
                    non_template_slice_start = idx + 1;
                    let desired_key = &template_string[template_var_start..idx];
                    match self.env.vars.get(desired_key) {
                        Some(val) => {
                            populated_string.push_str(val);
                        }
                        None => return Err(Error::UnknownVar(desired_key.to_string())),
                    }
                    state = UrlTemplateParser::Looking;
                }
                _ => {}
            }
        }

        if populated_string.is_empty() {
            Ok(Cow::Borrowed(template_string))
        } else {
            Ok(Cow::Owned(populated_string))
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
        self.handle.url(&self.populate_request_url(url.as_str())?)?;

        let mut header_list = curl::easy::List::new();
        for (key, value) in req.headers {
            header_list.append(&format!("{key}: {value}"))?;
        }

        self.handle.ssl_verify_peer(false)?;
        self.handle.ssl_verify_host(false)?;

        for flag in req.flags {}

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
                    CursorMoveDirection::Up => {
                        if self.header_paragraph_scroll >= 1 {
                            self.header_paragraph_scroll -= 1;
                        }
                    }
                    CursorMoveDirection::Down => {
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
                    self.editor = EditorInner::from(
                        JsonFormatter::format_output_data_into_lines(&self.output_data)
                            .unwrap_or(vec!["Json formatting failed".to_string()]),
                    );

                    self.editor.text_renderer = Some(JsonFormatter::json_tokenizer_spans);
                }
                _ => {
                    self.editor = String::from_utf8_lossy(&self.output_data)
                        .to_string()
                        .replace("\t", "  ")
                        .replace("\r\n", "\n")
                        .split('\n')
                        .map(|str| str.to_string())
                        .collect::<Vec<_>>()
                        .into();

                    self.editor.text_renderer = None
                }
            },
            Err(e) => self.error = Some(e),
        }
        None
    }

    fn move_cursor(&mut self, operator: CursorMovement) {
        if let Ok(movement) = TryInto::<CursorMoveDirection>::try_into(operator.clone()) {
            self.editor.move_cursor(movement);
            return;
        }

        let lines = self.editor.lines();
        let (row, col) = self.editor.cursor();
        match operator {
            CursorMovement::Until(char) => {
                let current_line = &lines[row as usize][col as usize..];
                let rest_of_current_line = &lines[row as usize][..col as usize];
                if let Some(idx) = current_line.find(char) {
                    self.editor.move_cursor(CursorMoveDirection::Jump(
                        row as u16,
                        (idx + rest_of_current_line.len()) as u16,
                    ))
                }
            }
            _ => {}
        }
    }
}

impl<'widget> StatefulWidgetRef for RequestExecutor {
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

impl<'widget> InputListener for RequestExecutor {
    fn handle_event(&mut self, e: crossterm::event::Event) -> Option<WidgetCommand> {
        match e {
            crossterm::event::Event::Key(key_event) => match key_event {
                KeyEvent {
                    code: KeyCode::Up, ..
                } => self.move_cursor_or_headers(CursorMovement::Regular(CursorMoveDirection::Up)),
                KeyEvent {
                    code: KeyCode::Down,
                    ..
                } => {
                    self.move_cursor_or_headers(CursorMovement::Regular(CursorMoveDirection::Down))
                }
                KeyEvent {
                    code: KeyCode::Left,
                    ..
                } => {
                    self.move_cursor_or_headers(CursorMovement::Regular(CursorMoveDirection::Back))
                }
                KeyEvent {
                    code: KeyCode::Right,
                    ..
                } => self
                    .move_cursor_or_headers(CursorMovement::Regular(CursorMoveDirection::Forward)),
                KeyEvent {
                    code: KeyCode::Char(ch),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => return editor::widget_common::move_widget_selection(ch),
                KeyEvent {
                    code: KeyCode::Char(ch),
                    ..
                } => {
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

                        keys::UP
                        | keys::DOWN
                        | keys::LEFT
                        | keys::RIGHT
                        | '$'
                        | '0'
                        | 'g'
                        | 'G'
                        | 'w'
                        | 'W'
                        | 'b'
                        | 'e'
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

impl<'widget> CurlmanWidget for RequestExecutor {
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

struct JsonFormatter {}

impl JsonFormatter {
    pub fn format_output_data_into_lines(input: &[u8]) -> Result<Vec<String>, FormatterError> {
        let input_str = CString::new(input).map_err(|_| FormatterError::InvalidInput)?;
        let input_jv = unsafe { jv_parse(input_str.as_ptr()) };
        let jv_kind = unsafe { jv_get_kind(input_jv) };

        if jv_kind == jv_kind_JV_KIND_INVALID {
            if unsafe { jv_invalid_has_msg(jv_copy(input_jv)) == 1 } {
                //let error = unsafe { jv_invalid_get_msg(input_jv) };
                //let jv_c_string_val = unsafe { jv_string_value(error) };
                //let jv_error_string = unsafe { CStr::from_ptr(jv_c_string_val) }.to_string_lossy();
                //unsafe { jv_free(error) }
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

    pub fn json_tokenizer_spans<'a>(
        lines: &'a Vec<String>,
        area: Rect,
        (row, col): (usize, usize),
        top_row: usize,
    ) -> (Vec<Line<'a>>, usize) {
        use std::mem::take;
        let colorscheme = get_default_output_colorscheme();
        let tokenized_lines = parse_request_json(lines);
        let width = area.width as usize;
        let height = area.height as usize;
        let mut lines = vec![];
        let mut line_spans = vec![];
        let mut overflow_lines = 0;

        for (idx, line) in tokenized_lines.into_iter().enumerate() {
            if idx < top_row || idx + overflow_lines >= height + top_row {
                continue;
            }

            let mut remaining_space = area.width as usize;
            let mut render_col_offset = 0;
            for token in line {
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
                        let color = token.get_color(&colorscheme);

                        (remaining_space, render_col_offset, overflow_lines) =
                            fit_tokens_into_editor(
                                text,
                                (row, col),
                                idx,
                                color,
                                width,
                                remaining_space,
                                render_col_offset,
                                overflow_lines,
                                &mut line_spans,
                                &mut lines,
                            );
                    }
                }
            }
            if idx == row && col == render_col_offset {
                line_spans.push(Span::raw(" ").reversed());
            }

            if !line_spans.is_empty() {
                lines.push(Line::from(take(&mut line_spans)));
            }
        }

        (lines, overflow_lines)
    }
}

mod tests {
    use crate::{editor::colors::get_default_output_colorscheme, executor::JsonFormatter};
    use jq_sys::{jq_init, jq_teardown};

    #[test]
    fn test_formatting_output_data_into_lines() {
        let mut jq_state = unsafe { jq_init() };
        if jq_state.is_null() {
            panic!("Couldn't initialize jq");
        }
        let input = "{\"name\" : \"John\", \"age\": 30}";
        let result = JsonFormatter::format_output_data_into_lines(input.as_bytes());

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
