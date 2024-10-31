//TODO?
//Add vertical scrolling of text?
//timeout for motions?

use colors::{get_default_colorscheme, Colorscheme};
use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
};

use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    text::{Line, Text},
    widgets::{Block, BorderType, List, Paragraph, StatefulWidgetRef},
};

use crate::{error::Error, keys, parser::parse_curlman_editor, types::RequestInfo, AppState};

pub mod colors {
    use ratatui::style::Color;

    pub struct Colorscheme {
        pub curl_color: Color,
        pub url_color: Color,
        pub param_key_color: Color,
        pub param_value_color: Color,
        pub separator_color: Color,
        pub unknown_color: Color,
    }

    pub fn get_default_colorscheme() -> Colorscheme {
        Colorscheme {
            curl_color: Color::Cyan,
            url_color: Color::Yellow,
            param_key_color: Color::Green,
            param_value_color: Color::Blue,
            separator_color: Color::White,
            unknown_color: Color::White,
        }
    }
}

pub enum VimMode {
    Normal,
    Insert,
}

#[derive(Debug)]
pub enum VimState {
    AwaitingFirstInput,
    AwaitingOperatorOperand,
    WritingCommand,
}

#[derive(Debug)]
pub enum CursorMovement {
    Up,
    Down,
    Left,
    Right,
    Until(char),
    Whole,
    UntilEndOfLine,
    UntilStartOfLine,
    UntilStartOfBuffer,
    UntilEndOfBuffer,
}

#[derive(Debug)]
pub enum VimMotion {
    Move(CursorMovement),
    InsertNewLineDown,
    Append,
    Insert,
    Delete(CursorMovement),
}

#[derive(Debug)]
pub enum VimCommand {
    Save,
    Quit,
}

pub enum VimAction {
    Motion(VimMotion),
    Command(VimCommand),
}

impl TryFrom<&str> for VimCommand {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "w" => Ok(Self::Save),
            "q" => Ok(Self::Quit),
            _ => Err(()),
        }
    }
}

impl TryFrom<char> for CursorMovement {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'k' => Ok(Self::Up),
            'j' => Ok(Self::Down),
            '$' => Ok(Self::UntilEndOfLine),
            '0' => Ok(Self::UntilStartOfLine),
            'h' => Ok(Self::Left),
            'l' => Ok(Self::Right),
            'd' | 'y' => Ok(Self::Whole),
            _ => Err(()),
        }
    }
}

impl TryFrom<char> for VimMotion {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'i' => Ok(Self::Insert),
            'a' => Ok(Self::Append),
            'k' => Ok(Self::Move(CursorMovement::Up)),
            'j' => Ok(Self::Move(CursorMovement::Down)),
            'h' => Ok(Self::Move(CursorMovement::Left)),
            'l' => Ok(Self::Move(CursorMovement::Right)),
            'o' => Ok(Self::InsertNewLineDown),
            '0' => Ok(Self::Move(CursorMovement::UntilStartOfLine)),
            '$' => Ok(Self::Move(CursorMovement::UntilEndOfLine)),
            'x' => Ok(Self::Delete(CursorMovement::Right)),
            _ => Err(()),
        }
    }
}

impl TryFrom<[char; 3]> for VimMotion {
    type Error = ();
    fn try_from(value: [char; 3]) -> Result<Self, Self::Error> {
        match value {
            ['f', second, ' '] => Ok(Self::Move(CursorMovement::Until(second))),
            ['d', 'd', ' '] => Ok(Self::Delete(CursorMovement::Whole)),
            ['d', 'f', ' '] => Err(()),
            ['d', 'f', third] => Ok(Self::Delete(CursorMovement::Until(third))),
            ['y', 'f', ' '] => Err(()),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct VimEditorState {
    motion_buffer: [char; 3],
    repeat_n: String,
    current_state: VimState,
    command_buff: String,
}

impl VimEditorState {
    fn clear(&mut self) {
        *self = Self::default();
    }
}

impl Default for VimEditorState {
    fn default() -> Self {
        VimEditorState {
            motion_buffer: [' ', ' ', ' '],
            current_state: VimState::AwaitingFirstInput,
            repeat_n: String::new(),
            command_buff: String::new(),
        }
    }
}

pub enum EditorMode {
    Vim {
        mode: VimMode,
        state: VimEditorState,
    },
}

pub struct Editor<'editor> {
    col: usize,
    row: usize,
    pub lines: Vec<String>,
    colorscheme: Colorscheme,
    block: Block<'editor>,
    tab_len: u8,
    current_mode: EditorMode,
    selected: bool,
}

impl<'editor> Editor<'editor> {
    pub fn new() -> Self {
        Self {
            block: Block::bordered().border_style(Style::new().red()),
            tab_len: 4,
            current_mode: EditorMode::Vim {
                mode: VimMode::Normal,
                state: VimEditorState {
                    repeat_n: String::new(),
                    motion_buffer: [' ', ' ', ' '],
                    current_state: VimState::AwaitingFirstInput,
                    command_buff: String::new(),
                },
            },
            selected: true,
            col: 0,
            row: 0,
            lines: vec![String::from("")],
            colorscheme: get_default_colorscheme(),
        }
    }

    fn get_editor_text(&self, area: Rect) -> Text {
        let (_, tokenized_lines) = parse_curlman_editor(&self.lines, &self.colorscheme).unwrap();

        let mut spans = Vec::new();
        let mut lines = Vec::new();

        let mut is_cursor_set = false;

        for (row_idx, line) in tokenized_lines.into_iter().enumerate() {
            let mut current_col = 0;
            for line_token in line {
                let line_token_len = line_token.get_str().len();
                let line_token_end = (current_col + line_token_len).checked_sub(1).unwrap_or(0);

                let cursor_has_to_be_set =
                    !is_cursor_set && row_idx == self.row && line_token_end >= self.col;

                match line_token {
                    crate::parser::Token::Curl(text, color)
                    | crate::parser::Token::Url(text, color)
                    | crate::parser::Token::ParamKey(text, color)
                    | crate::parser::Token::ParamValue(text, color)
                    | crate::parser::Token::Unknown(text, color)
                    | crate::parser::Token::Separator(text, color) => 'token_block: {
                        if !cursor_has_to_be_set {
                            spans.push(Span::raw(text).fg(color));
                            break 'token_block;
                        }
                        is_cursor_set = true;
                        let split_val = self.col - current_col;
                        let (before_cursor, at_and_before_cursor) = text.split_at(split_val);
                        spans.push(Span::raw(before_cursor).fg(color));
                        let (cursor_char, rest_of_span) =
                            (&at_and_before_cursor[0..1], &at_and_before_cursor[1..]);
                        spans.push(Span::raw(cursor_char).fg(color).reversed());
                        spans.push(Span::raw(rest_of_span).fg(color));
                    }
                    crate::parser::Token::Whitespace(text) => 'whitespace_block: {
                        if !cursor_has_to_be_set {
                            spans.push(Span::raw(text));
                            break 'whitespace_block;
                        }

                        is_cursor_set = true;

                        let split_val = self.col - current_col;
                        let (before_cursor, at_and_before_cursor) = text.split_at(split_val);

                        spans.push(Span::raw(before_cursor));

                        let (cursor_char, rest_of_span) =
                            (&at_and_before_cursor[0..1], &at_and_before_cursor[1..]);

                        spans.push(Span::raw(cursor_char).reversed());
                        spans.push(Span::raw(rest_of_span));
                    }
                };

                current_col += line_token_len;
            }

            if row_idx == self.row && self.col == self.lines[self.row].len() {
                spans.push(Span::raw(" ").reversed());
            }

            lines.push(Line::from(std::mem::take(&mut spans)));
        }

        Text::from(lines)
    }

    fn handle_input(&mut self, event: Event) -> Option<WidgetCommand> {
        match event {
            Event::Key(key_event) => match self.current_mode {
                EditorMode::Vim { ref mode, .. } => match mode {
                    VimMode::Normal => return self.handle_vim_motions(key_event),
                    VimMode::Insert => {
                        self.handle_key_events(key_event);
                    }
                },
            },
            Event::Paste(e) => self.lines[self.row].insert_str(self.col, &e),
            _ => {}
        };

        None
    }

    fn handle_key_events(&mut self, event: KeyEvent) {
        match event {
            KeyEvent {
                code: KeyCode::Char(ch),
                kind: KeyEventKind::Press | KeyEventKind::Repeat,
                ..
            } => {
                self.insert_char(ch);
            }
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => self.insert_newline(),
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => self.backspace_delete(),
            KeyEvent {
                code: KeyCode::Tab, ..
            } => self.insert_tab(),
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_cursor(CursorMovement::Up),
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_cursor(CursorMovement::Down),
            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_cursor(CursorMovement::Left),
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_cursor(CursorMovement::Right),
            KeyEvent {
                code: KeyCode::Esc,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                let EditorMode::Vim { mode, .. } = &mut self.current_mode;
                if !matches!(mode, VimMode::Insert) {
                    return;
                }
                self.switch_to_normal_mode()
            }
            _ => {}
        }
    }

    fn handle_commands(&mut self, command: VimCommand) -> Option<WidgetCommand> {
        match command {
            VimCommand::Save => {
                return Some(WidgetCommand::Save {
                    text: String::new(),
                });
            }
            VimCommand::Quit => {
                return Some(WidgetCommand::Quit);
            }
        }
    }

    fn handle_motions(&mut self, motion: VimMotion) {
        match motion {
            VimMotion::Move(operator) => self.move_cursor(operator),
            VimMotion::Append => {
                self.switch_to_insert_mode();
            }
            VimMotion::Insert => {
                self.switch_to_insert_mode();
            }
            VimMotion::Delete(vim_operator) => match vim_operator {
                CursorMovement::Up => {}
                CursorMovement::Down => {}
                CursorMovement::Left => {}
                CursorMovement::Right => {
                    self.backspace_delete();
                }
                CursorMovement::Until(char) => {}
                CursorMovement::Whole => {}
                CursorMovement::UntilEndOfLine => {}
                CursorMovement::UntilStartOfLine => {}
                CursorMovement::UntilStartOfBuffer => {}
                CursorMovement::UntilEndOfBuffer => {}
            },
            VimMotion::InsertNewLineDown => {}
            _ => {}
        }
    }

    fn handle_vim_motions(&mut self, e: KeyEvent) -> Option<WidgetCommand> {
        let EditorMode::Vim { ref mode, state } = &mut self.current_mode;

        if !matches!(mode, VimMode::Normal) {
            return None;
        }

        let mut vim_command_to_exec = None;

        match e {
            KeyEvent {
                code: KeyCode::Esc, ..
            } => match state.current_state {
                VimState::WritingCommand => {
                    state.clear();
                }
                _ => {}
            },
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => match state.current_state {
                VimState::WritingCommand => {
                    if !state.command_buff.is_empty() {
                        state.command_buff.pop();
                    }
                }
                _ => {}
            },
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => match state.current_state {
                VimState::WritingCommand => 'exec_command: {
                    let written_command: Result<VimCommand, _> =
                        state.command_buff.as_str().try_into();

                    let Ok(command) = written_command else {
                        state.current_state = VimState::AwaitingFirstInput;
                        break 'exec_command;
                    };

                    vim_command_to_exec = Some(VimAction::Command(command));
                }
                _ => {}
            },
            KeyEvent {
                code: KeyCode::Char(ch),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => match ch {
                keys::UP => {
                    return Some(WidgetCommand::MoveWidgetSelection {
                        direction: keys::Direction::Up,
                    })
                }
                keys::DOWN => {
                    return Some(WidgetCommand::MoveWidgetSelection {
                        direction: keys::Direction::Down,
                    })
                }
                keys::LEFT => {
                    return Some(WidgetCommand::MoveWidgetSelection {
                        direction: keys::Direction::Left,
                    })
                }
                keys::RIGHT => {
                    return Some(WidgetCommand::MoveWidgetSelection {
                        direction: keys::Direction::Right,
                    })
                }
                _ => {}
            },
            KeyEvent {
                code: KeyCode::Char(ch),
                ..
            } => match state.current_state {
                VimState::AwaitingFirstInput => 'awaiting_first_input: {
                    if ch == ':' {
                        state.current_state = VimState::WritingCommand;
                        state.motion_buffer = [' ', ' ', ' '];
                        break 'awaiting_first_input;
                    }

                    if ch != '0' && ch.is_ascii_digit() {
                        state.repeat_n.push(ch)
                    } else if let Ok(vim_motion) = TryInto::<VimMotion>::try_into(ch) {
                        state.clear();
                        vim_command_to_exec = Some(VimAction::Motion(vim_motion));
                    } else {
                        state.motion_buffer[0] = ch;
                        state.current_state = VimState::AwaitingOperatorOperand;
                    }
                }
                VimState::AwaitingOperatorOperand => 'operator_block: {
                    if ch == ':' {
                        state.current_state = VimState::WritingCommand;
                        state.motion_buffer = [' ', ' ', ' '];
                        break 'operator_block;
                    }

                    if state.motion_buffer[1] == ' ' {
                        state.motion_buffer[1] = ch
                    } else {
                        state.motion_buffer[2] = ch
                    }

                    match TryInto::<VimMotion>::try_into(state.motion_buffer) {
                        Ok(command) => {
                            vim_command_to_exec = Some(VimAction::Motion(command));
                            state.clear();
                        }
                        Err(_) => {
                            if state.motion_buffer[2] != ' ' {
                                state.clear();
                            }
                        }
                    }
                }
                VimState::WritingCommand => state.command_buff.push(ch),
            },
            _ => {}
        };

        if let Some(command) = vim_command_to_exec {
            match command {
                VimAction::Motion(vim_motion) => self.handle_motions(vim_motion),
                VimAction::Command(vim_command) => {
                    let res = self.handle_commands(vim_command);
                    self.switch_to_normal_mode();
                    return res;
                }
            }
        }

        None
    }

    fn switch_to_normal_mode(&mut self) {
        let EditorMode::Vim { mode, state } = &mut self.current_mode;
        *mode = VimMode::Normal;
        *state = VimEditorState::default();
    }

    fn switch_to_insert_mode(&mut self) {
        let EditorMode::Vim { mode, state } = &mut self.current_mode;
        *mode = VimMode::Insert;
        *state = VimEditorState::default();
    }

    fn insert_char(&mut self, ch: char) {
        let line = &mut self.lines[self.row];

        let insert_idx = line
            .char_indices()
            .nth(self.col)
            .map(|(i, _)| i)
            .unwrap_or(line.len());

        line.insert(insert_idx, ch);
        self.col += 1;
    }

    fn insert_newline(&mut self) {
        let line = &mut self.lines[self.row];
        let offset = line
            .char_indices()
            .nth(self.col)
            .map(|(i, _)| i)
            .unwrap_or(line.len());

        let next_line = line[offset..].to_string();
        line.truncate(offset);

        self.lines.insert(self.row + 1, next_line);
        self.row += 1;
        self.col = 0;
    }

    fn insert_tab(&mut self) {}

    fn delete_newline(&mut self) {
        if self.row == 0 {
            return;
        }

        let line = self.lines.remove(self.row);
        let prev_line = &mut self.lines[self.row - 1];

        self.row -= 1;
        self.col = prev_line.chars().count();
        prev_line.push_str(&line);
    }

    fn move_cursor(&mut self, operator: CursorMovement) {
        if let Some((new_row, new_col)) = self.get_next_cursor_position(operator) {
            self.col = new_col;
            self.row = new_row;
        }
    }

    fn get_next_cursor_position(&self, operator: CursorMovement) -> Option<(usize, usize)> {
        fn fit_col(col: usize, line: &str) -> usize {
            std::cmp::min(col, line.chars().count())
        }

        match operator {
            CursorMovement::Right if self.col >= self.lines[self.row].chars().count() => {
                (self.row + 1 < self.lines.len()).then(|| (self.row + 1, 0))
            }
            CursorMovement::Right => Some((self.row, self.col + 1)),
            CursorMovement::Left if self.col == 0 => {
                let row = self.row.checked_sub(1)?;
                Some((row, self.lines[row].chars().count()))
            }
            CursorMovement::Left => Some((self.row, self.col - 1)),
            CursorMovement::Up => {
                let row = self.row.checked_sub(1)?;
                Some((row, fit_col(self.col, &self.lines[row])))
            }
            CursorMovement::Down => Some((
                self.row + 1,
                fit_col(self.col, self.lines.get(self.row + 1)?),
            )),
            CursorMovement::UntilStartOfLine => Some((self.row, 0)),
            CursorMovement::UntilEndOfLine => {
                Some((self.row, self.lines[self.row].chars().count()))
            }
            CursorMovement::UntilStartOfBuffer => Some((0, fit_col(self.col, &self.lines[0]))),
            CursorMovement::UntilEndOfBuffer => {
                let row = self.lines.len() - 1;
                Some((row, fit_col(self.col, &self.lines[row])))
            }
            CursorMovement::Until(_) => None,
            CursorMovement::Whole => None,
        }
    }

    fn backspace_delete(&mut self) {
        if self.col == 0 {
            return self.delete_newline();
        }

        let line = &mut self.lines[self.row];

        if let Some((offset, _)) = line.char_indices().nth(self.col - 1) {
            line.remove(offset);
            self.col -= 1;
        }
    }

    fn forward_delete(&mut self) {}
}

impl<'editor> StatefulWidgetRef for Editor<'editor> {
    #[doc = " State associated with the stateful widget."]
    #[doc = ""]
    #[doc = " If you don\'t need this then you probably want to implement [`WidgetRef`] instead."]
    type State = AppState;

    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom stateful widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer, _: &mut Self::State) {
        const COMMAND_BAR_HEIGHT: u16 = 1;
        let text_area = self.block.inner(area);
        self.block.clone().render(area, buf);
        let editor_area_height = text_area.height.saturating_sub(COMMAND_BAR_HEIGHT);

        let editor_area = Rect {
            x: text_area.x,
            y: text_area.y,
            width: text_area.width,
            height: editor_area_height,
        };
        let command_bar_area = Rect {
            x: text_area.x,
            y: text_area.y + editor_area_height, // Position immediately below the editor area
            width: text_area.width,
            height: 1,
        };

        let inner_editor_content = Paragraph::new(self.get_editor_text(editor_area));
        inner_editor_content.render(editor_area, buf);
        let EditorMode::Vim { ref state, .. } = self.current_mode;
        const MOTION_BUFFER_RENDER_LEN: i16 = 16;

        let status_bar_str = if matches!(state.current_state, VimState::WritingCommand) {
            format!(":{}", &state.command_buff)
        } else {
            let number_of_spaces = command_bar_area.width as isize
                - (state.repeat_n.len() as isize + MOTION_BUFFER_RENDER_LEN as isize);

            format!(
                "{}{}{:?}",
                " ".repeat(number_of_spaces as usize),
                state.repeat_n,
                state.motion_buffer
            )
        };

        let status_bar_content = Line::from(status_bar_str);

        status_bar_content.render(command_bar_area, buf);
    }
}

#[derive(Debug)]
pub enum WidgetCommand {
    MoveWidgetSelection { direction: keys::Direction },
    Save { text: String },
    MoveRequestSelection { new_idx: usize },
    Quit,
}

pub trait InputListener {
    fn handle_event(&mut self, e: Event) -> Option<WidgetCommand>;
}

pub trait CurlmanWidget: InputListener + StatefulWidgetRef {
    fn toggle_selected(&mut self);
    fn update_shared_state(&mut self, new_state: &AppState) -> Result<(), Error>;
}

impl<'editor> InputListener for Editor<'editor> {
    fn handle_event(&mut self, e: Event) -> Option<WidgetCommand> {
        self.handle_input(e)
    }
}

impl<'editor> CurlmanWidget for Editor<'editor> {
    fn toggle_selected(&mut self) {
        self.selected = !self.selected;

        if self.selected {
            self.block = Block::bordered().border_style(Style::new().red());
        } else {
            self.block = Block::bordered();
        }
    }

    fn update_shared_state(&mut self, _: &AppState) -> Result<(), Error> {
        Ok(())
    }
}

pub struct RequestBrowser<'browser> {
    block: Block<'browser>,
    requests: Option<Vec<RequestInfo>>,
    pub selected_request_idx: Option<usize>,
    selected: bool,
}

impl<'browser> StatefulWidgetRef for RequestBrowser<'browser> {
    #[doc = " State associated with the stateful widget."]
    #[doc = ""]
    #[doc = " If you don\'t need this then you probably want to implement [`WidgetRef`] instead."]
    type State = AppState;

    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom stateful widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        let Some(requests) = &self.requests else {
            self.block.clone().render(area, buf);
            return;
        };

        let requests = List::new(
            requests
                .iter()
                .enumerate()
                .map(|(idx, _)| format!("Request {idx}")),
        )
        .block(self.block.clone())
        .direction(ratatui::widgets::ListDirection::TopToBottom)
        .highlight_symbol("-> ")
        .highlight_style(Style::new().reversed());
        requests.render_ref(area, buf, &mut state.list_state);
    }
}

impl<'browser> InputListener for RequestBrowser<'browser> {
    fn handle_event(&mut self, e: Event) -> Option<WidgetCommand> {
        match e {
            Event::FocusGained => todo!(),
            Event::FocusLost => todo!(),
            Event::Key(key_event) => match key_event {
                KeyEvent {
                    code: KeyCode::Char(ch),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => match ch {
                    keys::UP => {
                        return Some(WidgetCommand::MoveWidgetSelection {
                            direction: keys::Direction::Up,
                        })
                    }
                    keys::DOWN => {
                        return Some(WidgetCommand::MoveWidgetSelection {
                            direction: keys::Direction::Down,
                        })
                    }
                    keys::LEFT => {
                        return Some(WidgetCommand::MoveWidgetSelection {
                            direction: keys::Direction::Left,
                        })
                    }
                    keys::RIGHT => {
                        return Some(WidgetCommand::MoveWidgetSelection {
                            direction: keys::Direction::Right,
                        })
                    }
                    _ => {}
                },
                KeyEvent {
                    code: KeyCode::Char(ch),
                    ..
                } => {
                    let (Some(idx), Some(requests)) =
                        (&mut self.selected_request_idx, &self.requests)
                    else {
                        return None;
                    };

                    if requests.is_empty() {
                        return None;
                    };

                    match ch {
                        keys::UP => {
                            if *idx > 0 {
                                *idx -= 1;
                                return Some(WidgetCommand::MoveRequestSelection { new_idx: *idx });
                            }
                        }
                        keys::DOWN => {
                            if *idx < requests.len() {
                                *idx += 1;
                                return Some(WidgetCommand::MoveRequestSelection { new_idx: *idx });
                            }
                        }
                        _ => {}
                    };
                }
                _ => {}
            },
            Event::Mouse(_) => todo!(),
            Event::Paste(_) => todo!(),
            Event::Resize(_, _) => todo!(),
        };

        None
    }
}

impl<'browser> Default for RequestBrowser<'browser> {
    fn default() -> Self {
        RequestBrowser {
            block: get_round_bordered_box(),
            requests: None,
            selected: false,
            selected_request_idx: None,
        }
    }
}

impl<'browser> From<Vec<RequestInfo>> for RequestBrowser<'browser> {
    fn from(value: Vec<RequestInfo>) -> Self {
        Self {
            selected_request_idx: if !value.is_empty() { Some(0) } else { None },
            requests: Some(value),
            ..Self::default()
        }
    }
}

impl<'browser> CurlmanWidget for RequestBrowser<'browser> {
    fn toggle_selected(&mut self) {
        self.selected = !self.selected;
        if self.selected {
            self.block = Block::bordered().border_style(Style::new().red());
        } else {
            self.block = Block::bordered();
        }
    }
    fn update_shared_state(&mut self, new_state: &AppState) -> Result<(), Error> {
        self.requests = Some(new_state.requests.clone());
        Ok(())
    }
}

pub fn get_round_bordered_box() -> Block<'static> {
    Block::bordered().border_type(BorderType::Rounded)
}
