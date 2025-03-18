use crate::{
    cursor_movements::{CursorMoveDirection, CursorMovement},
    error::Error,
    keys,
    parser::{parse_curlman_editor, CurlmanToken},
    types::RequestInfo,
    AppState,
};
use arboard::Clipboard;
use colors::{get_default_editor_colorscheme, EditorColorscheme};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    widgets::{Block, BorderType, List, StatefulWidgetRef},
};
use widget_common::{fit_tokens_into_editor, EditorInner};

pub mod widget_common {
    use std::sync::atomic::{AtomicU16, Ordering};

    use ratatui::{
        buffer::Buffer,
        layout::Rect,
        style::{Color, Stylize},
        text::{Line, Span, Text},
        widgets::{Widget, WidgetRef},
    };

    use super::WidgetCommand;
    use crate::{cursor_movements::CursorMoveDirection, keys};

    pub struct EditorInner {
        pub lines: Vec<String>,
        pub cursor: (usize, usize),
        pub top_row: usize,
        pub height: AtomicU16,
        pub overflow_lines: AtomicU16,
        pub text_renderer:
            Option<fn(&Vec<String>, Rect, (usize, usize), usize) -> (Vec<Line<'_>>, usize)>,
    }

    impl WidgetRef for EditorInner {
        #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
        #[doc = " to implement a custom widget."]
        fn render_ref(&self, area: Rect, buf: &mut Buffer) {
            self.height.store(area.height, Ordering::Relaxed);

            let (lines, overflow_line_count) = match self.text_renderer {
                Some(tokenizer) => tokenizer(&self.lines, area, self.cursor, self.top_row),
                None => self.wrap_editor_lines_no_color(area),
            };

            self.overflow_lines
                .store(overflow_line_count as u16, Ordering::Relaxed);

            Text::from(lines).render(area, buf)
        }
    }

    impl EditorInner {
        pub fn new() -> Self {
            Self {
                text_renderer: None,
                lines: Vec::new(),
                cursor: (0, 0),
                top_row: 0,
                height: AtomicU16::new(0),
                overflow_lines: AtomicU16::new(0),
            }
        }

        pub fn with_renderer(
            self,
            text_renderer: fn(&Vec<String>, Rect, (usize, usize), usize) -> (Vec<Line<'_>>, usize),
        ) -> Self {
            Self {
                text_renderer: Some(text_renderer),
                ..self
            }
        }

        pub fn insert_char(&mut self, c: char) {
            if c == '\n' || c == '\r' {
                self.insert_newline();
                return;
            }

            let (row, col) = self.cursor;
            let line = &mut self.lines[row];

            let i = line
                .char_indices()
                .nth(col)
                .map(|(i, _)| i)
                .unwrap_or(line.len());

            line.insert(i, c);
            self.cursor.1 += 1;
        }

        pub fn insert_newline(&mut self) {
            let (row, col) = self.cursor;
            let line = &mut self.lines[row];
            let offset = line
                .char_indices()
                .nth(col)
                .map(|(i, _)| i)
                .unwrap_or(line.len());
            let next_line = line[offset..].to_string();
            line.truncate(offset);

            self.lines.insert(row + 1, next_line);
            self.cursor = (row + 1, 0);
            self.adjust_viewport();
        }

        fn spaces(size: u8) -> &'static str {
            const SPACES: &str = "                                                                                                                                                                                                                                                                ";
            &SPACES[..size as usize]
        }

        pub fn insert_tab(&mut self) {
            const TAB_LEN: usize = 2;
            let (row, col) = self.cursor;

            let width: usize = self.lines[row]
                .chars()
                .take(col)
                .map(|c| c.len_utf8())
                .sum();

            let len = TAB_LEN - (width % TAB_LEN);
            self.insert_piece(Self::spaces(len as u8).to_string())
        }

        fn insert_piece(&mut self, s: String) {
            let (row, col) = self.cursor;
            let line = &mut self.lines[row];
            let i = line
                .char_indices()
                .nth(col)
                .map(|(i, _)| i)
                .unwrap_or(line.len());
            line.insert_str(i, &s);

            self.cursor.1 += s.chars().count();
        }

        fn adjust_viewport(&mut self) {
            let (row, _) = self.cursor;
            let height = self.height.load(Ordering::Relaxed) as usize;
            let overflow_line_count = self.overflow_lines.load(Ordering::Relaxed) as usize;

            if row < self.top_row {
                self.top_row = row;
            } else if row + overflow_line_count >= self.top_row + height {
                self.top_row = (row + overflow_line_count).saturating_sub(height - 1);
            }
        }

        pub fn move_cursor(&mut self, direction: CursorMoveDirection) {
            if let Some(cursor) = direction.next_cursor(self.cursor, &self.lines) {
                self.cursor = cursor;
                self.adjust_viewport();
            }
        }

        fn delete_newline(&mut self) {
            let (row, _) = self.cursor;
            if row == 0 {
                return;
            }

            let line = self.lines.remove(row);
            let prev_line = &mut self.lines[row - 1];

            self.cursor = (row - 1, prev_line.chars().count());
            prev_line.push_str(&line);

            self.adjust_viewport();
        }

        pub fn delete_char(&mut self) {
            let (row, col) = self.cursor;
            if col == 0 {
                return self.delete_newline();
            }

            let line = &mut self.lines[row];
            if let Some((offset, _)) = line.char_indices().nth(col - 1) {
                line.remove(offset);
                self.cursor.1 -= 1;
            }
        }

        pub fn cursor(&self) -> (usize, usize) {
            self.cursor
        }

        pub fn lines(&self) -> &Vec<String> {
            &self.lines
        }

        pub fn wrap_editor_lines_no_color(&self, area: Rect) -> (Vec<Line<'_>>, usize) {
            use std::mem::take;
            let (height, width) = (area.height as usize, area.width as usize);
            let (row, col) = self.cursor;
            let mut overflow_lines = 0;
            let mut lines = vec![];
            let mut line_spans = vec![];

            for (idx, line) in self.lines.iter().enumerate() {
                if idx < self.top_row || idx + overflow_lines >= self.top_row + height {
                    continue;
                }

                let mut render_col_offset = 0;

                (_, render_col_offset, overflow_lines) = fit_tokens_into_editor(
                    line,
                    (row, col),
                    idx,
                    Color::White,
                    width,
                    width,
                    render_col_offset,
                    overflow_lines,
                    &mut line_spans,
                    &mut lines,
                );

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

    fn split_token(token_str: &str, remaining_space: usize) -> (&str, &str) {
        let cutoff_char = token_str.char_indices().take(remaining_space + 1).last();

        if let Some((end_idx, _)) = cutoff_char {
            return (&token_str[..end_idx], &token_str[end_idx..]);
        }

        unreachable!();
    }

    enum TokenWrap<'text> {
        NoOverflow((Vec<Span<'text>>, usize)),
        Overflow((Vec<Span<'text>>, Vec<Vec<Span<'text>>>, usize)),
    }

    fn separate_cursor_slice(text: &str) -> Option<(&str, &str)> {
        if text.is_empty() {
            return None;
        }

        if let Some((_, ch)) = text.char_indices().take(1).last() {
            return Some((&text[..ch.len_utf8()], &text[ch.len_utf8()..]));
        }

        None
    }

    fn separate_cursor_slice_and_append<'editor_token>(
        at_and_after_cursor: &'editor_token str,
        mut remaining_space: usize,
        mut overflow_lines: usize,
        width: usize,
        color: Color,
        lines: &mut Vec<Line<'editor_token>>,
        line_spans: &mut Vec<Span<'editor_token>>,
    ) -> (usize, usize) {
        use std::mem::take;
        match separate_cursor_slice(at_and_after_cursor) {
            Some((cursor_str, rest)) => {
                if remaining_space < 1 {
                    lines.push(Line::from(take(line_spans)));
                    line_spans.push(Span::raw(cursor_str).reversed());
                    remaining_space = width - 1;
                } else {
                    line_spans.push(Span::raw(cursor_str).reversed());
                    remaining_space -= 1;
                }

                (remaining_space, overflow_lines) = fit_text_and_append_to_lines(
                    rest,
                    remaining_space,
                    overflow_lines,
                    color,
                    width,
                    line_spans,
                    lines,
                );

                (remaining_space, overflow_lines)
            }
            None => (remaining_space, overflow_lines),
        }
    }

    fn fit_text_and_append_to_lines<'editor_token>(
        text: &'editor_token str,
        remaining_space: usize,
        overflow_lines: usize,
        color: Color,
        width: usize,
        line_spans: &mut Vec<Span<'editor_token>>,
        lines: &mut Vec<Line<'editor_token>>,
    ) -> (usize, usize) {
        let token_fit_res = fit_token_in_remaining_space(text, remaining_space, color, width);
        extend_lines_and_line_spans(token_fit_res, overflow_lines, line_spans, lines)
    }

    fn fit_token_in_remaining_space(
        text: &str,
        remaining_space: usize,
        color: Color,
        width: usize,
    ) -> TokenWrap<'_> {
        if remaining_space.checked_sub(text.chars().count()).is_none() {
            let (curr_line_chunk, mut next_line_chunk) = split_token(text, remaining_space);
            let curr_line_span = Span::raw(curr_line_chunk).fg(color);
            let mut overflowed_lines = vec![];
            while let Some((idx, _)) = next_line_chunk.char_indices().nth(width) {
                overflowed_lines.push(vec![Span::raw(&next_line_chunk[..idx]).fg(color)]);
                next_line_chunk = &next_line_chunk[idx..];
            }

            overflowed_lines.push(vec![Span::raw(next_line_chunk).fg(color)]);
            TokenWrap::Overflow((
                vec![curr_line_span],
                overflowed_lines,
                width - next_line_chunk.chars().count(),
            ))
        } else {
            let line_span = Span::raw(text).fg(color);
            TokenWrap::NoOverflow((vec![line_span], remaining_space - text.chars().count()))
        }
    }

    fn extend_lines_and_line_spans<'editor_token>(
        token_fit_res: TokenWrap<'editor_token>,
        overflow_line_count: usize,
        line_spans: &mut Vec<Span<'editor_token>>,
        lines: &mut Vec<Line<'editor_token>>,
    ) -> (usize, usize) {
        use std::mem::take;
        match token_fit_res {
            TokenWrap::NoOverflow((line_span, space_left)) => {
                line_spans.extend(line_span);
                (space_left, overflow_line_count)
            }
            TokenWrap::Overflow((line_span, mut overflow_lines, space_left)) => {
                line_spans.extend(line_span);
                lines.push(Line::from(take(line_spans)));
                let token_overflowed_lines_len = overflow_lines.len();

                if let Some(tail) = overflow_lines.pop() {
                    for line in overflow_lines {
                        lines.push(Line::from(line))
                    }

                    line_spans.extend(tail);
                }

                (space_left, overflow_line_count + token_overflowed_lines_len)
            }
        }
    }

    pub fn fit_tokens_into_editor<'editor_token>(
        text: &'editor_token str,
        (row, col): (usize, usize),
        row_idx: usize,
        color: Color,
        width: usize,
        mut remaining_space: usize,
        mut render_col_offset: usize,
        mut overflow_lines: usize,
        line_spans: &mut Vec<Span<'editor_token>>,
        lines: &mut Vec<Line<'editor_token>>,
    ) -> (usize, usize, usize) {
        let text_len = text.len();
        if row == row_idx && col >= render_col_offset && render_col_offset + text_len > col {
            let cursor_col_relative = col - render_col_offset;
            let (before_cursor, at_and_after_cursor) = text.split_at(cursor_col_relative);

            (remaining_space, overflow_lines) = fit_text_and_append_to_lines(
                before_cursor,
                remaining_space,
                overflow_lines,
                color,
                width,
                line_spans,
                lines,
            );

            (remaining_space, overflow_lines) = separate_cursor_slice_and_append(
                at_and_after_cursor,
                remaining_space,
                overflow_lines,
                width,
                color,
                lines,
                line_spans,
            )
        } else {
            (remaining_space, overflow_lines) = fit_text_and_append_to_lines(
                text,
                remaining_space,
                overflow_lines,
                color,
                width,
                line_spans,
                lines,
            )
        }

        render_col_offset += text_len;
        (remaining_space, render_col_offset, overflow_lines)
    }

    impl From<Vec<String>> for EditorInner {
        fn from(value: Vec<String>) -> Self {
            Self {
                overflow_lines: AtomicU16::new(0),
                top_row: 0,
                text_renderer: None,
                lines: value,
                cursor: (0, 0),
                height: AtomicU16::new(0),
            }
        }
    }

    pub fn move_widget_selection(pressed: char) -> Option<WidgetCommand> {
        match pressed {
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
            _ => None,
        }
    }
}

pub mod colors {
    use ratatui::style::Color;

    pub struct EditorColorscheme {
        pub curl_color: Color,
        pub url_color: Color,
        pub param_key_color: Color,
        pub param_value_color: Color,
        pub separator_color: Color,
        pub unknown_color: Color,
    }

    pub struct JsonOutputColorscheme {
        pub object_bracket_color: Color,
        pub array_bracket_color: Color,
        pub name_separator_color: Color,
        pub value_separator_color: Color,
        pub literal_color: Color,
        pub invalid_color: Color,
        pub string_color: Color,
        pub identifier_color: Color,
    }

    pub fn get_default_editor_colorscheme() -> EditorColorscheme {
        EditorColorscheme {
            curl_color: Color::Cyan,
            url_color: Color::Yellow,
            param_key_color: Color::Green,
            param_value_color: Color::Blue,
            separator_color: Color::White,
            unknown_color: Color::White,
        }
    }

    pub fn get_default_output_colorscheme() -> JsonOutputColorscheme {
        JsonOutputColorscheme {
            object_bracket_color: Color::White,
            array_bracket_color: Color::White,
            name_separator_color: Color::White,
            value_separator_color: Color::White,
            literal_color: Color::White,
            string_color: Color::Green,
            invalid_color: Color::White,
            identifier_color: Color::Blue,
        }
    }
}

pub enum VimMode {
    Normal,
    Insert,
}

impl VimMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            VimMode::Normal => "NORMAL",
            VimMode::Insert => "INSERT",
        }
    }
}

#[derive(Debug)]
pub enum VimState {
    AwaitingFirstInput,
    AwaitingOperatorOperand,
    WritingCommand,
}

#[derive(Debug)]
pub enum VimMotion {
    Move(CursorMovement),
    InsertNewLineDown,
    Append,
    PasteClipboard,
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
            'k' => Ok(Self::Regular(CursorMoveDirection::Up)),
            'j' => Ok(Self::Regular(CursorMoveDirection::Down)),
            '$' => Ok(Self::Regular(CursorMoveDirection::End)),
            '0' => Ok(Self::Regular(CursorMoveDirection::Head)),
            'h' => Ok(Self::Regular(CursorMoveDirection::Back)),
            'l' => Ok(Self::Regular(CursorMoveDirection::Forward)),
            'g' => Ok(Self::Regular(CursorMoveDirection::Top)),
            'G' => Ok(Self::Regular(CursorMoveDirection::Bottom)),
            'e' => Ok(Self::Regular(CursorMoveDirection::WordEnd)),
            'w' => Ok(Self::Regular(CursorMoveDirection::WordForward)),
            'b' => Ok(Self::Regular(CursorMoveDirection::WordBack)),
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
            'p' => Ok(Self::PasteClipboard),
            'k' => Ok(Self::Move(CursorMovement::Regular(CursorMoveDirection::Up))),
            'j' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Down,
            ))),
            'h' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Back,
            ))),
            'l' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Forward,
            ))),
            'o' => Ok(Self::InsertNewLineDown),
            '0' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Head,
            ))),
            'G' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Bottom,
            ))),
            '$' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::End,
            ))),
            'x' => Ok(Self::Delete(CursorMovement::Regular(
                CursorMoveDirection::Back,
            ))),
            'e' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::WordEnd,
            ))),
            'w' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::WordForward,
            ))),
            'b' => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::WordBack,
            ))),
            _ => Err(()),
        }
    }
}

impl TryFrom<[char; 3]> for VimMotion {
    type Error = ();
    fn try_from(value: [char; 3]) -> Result<Self, Self::Error> {
        match value {
            ['f', second, ' '] => Ok(Self::Move(CursorMovement::Until(second))),
            ['d', 'd', ' '] => Ok(Self::Delete(CursorMovement::WholeLine)),
            ['d', 'f', ' '] => Err(()),
            ['d', 'f', third] => Ok(Self::Delete(CursorMovement::Until(third))),
            ['y', 'f', ' '] => Err(()),
            ['g', 'g', ' '] => Ok(Self::Move(CursorMovement::Regular(
                CursorMoveDirection::Top,
            ))),
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
    pub editor: EditorInner,
    colorscheme: EditorColorscheme,
    block: Block<'editor>,
    current_mode: EditorMode,
    selected: bool,
}

impl<'editor> Editor<'editor> {
    pub fn new(start_state: Vec<String>) -> Self {
        Self {
            block: get_round_bordered_box()
                .border_style(Style::new().red())
                .title("Editor"),
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
            colorscheme: get_default_editor_colorscheme(),
            editor: Into::<EditorInner>::into(start_state)
                .with_renderer(Self::editor_tokenizer_spans),
        }
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
            } => self.editor.insert_char(ch),
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => self.editor.insert_newline(),
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => {
                self.editor.delete_char();
            }
            KeyEvent {
                code: KeyCode::Tab, ..
            } => {
                self.editor.insert_tab();
            }
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMoveDirection::Up),
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMoveDirection::Down),
            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMoveDirection::Back),
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMoveDirection::Forward),
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
                    text: self.editor.lines().join("\n"),
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
                CursorMovement::Until(char) => {}
                CursorMovement::Regular(cursor_move) => {}
                CursorMovement::WholeLine => {}
            },
            VimMotion::InsertNewLineDown => {
                self.move_cursor(CursorMovement::Regular(CursorMoveDirection::End));
                self.editor.insert_newline();
                self.switch_to_insert_mode();
            }
            VimMotion::PasteClipboard => {
                let clipboard_res = Clipboard::new();
                match clipboard_res {
                    Ok(mut clipboard) => {
                        if let Ok(text) = clipboard.get_text() {
                            for ch in text.replace("\r\n", "\n").chars() {
                                self.editor.insert_char(ch);
                            }
                        }
                    }
                    Err(e) => {
                        for ch in format!("Clipboard not supported : {e:?}").chars() {
                            self.editor.insert_char(ch);
                        }
                    }
                };
            }
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
                VimState::AwaitingOperatorOperand => state.clear(),
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
                        state.command_buff.clear();
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
            } => return widget_common::move_widget_selection(ch),
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
                if let Some(idx) = current_line.find(char) {
                    self.editor
                        .move_cursor(CursorMoveDirection::Jump(row as u16, idx as u16))
                }
            }
            _ => {}
        }
    }

    fn editor_tokenizer_spans<'a>(
        lines: &'a Vec<String>,
        area: Rect,
        (row, col): (usize, usize),
        top_row: usize,
    ) -> (Vec<Line<'a>>, usize) {
        use std::mem::take;
        let colorscheme = get_default_editor_colorscheme();
        let tokenized_lines = parse_curlman_editor(lines);

        let width = area.width as usize;
        let height = area.height as usize;
        let mut overflow_lines = 0;
        let mut lines = vec![];
        let mut line_spans = vec![];

        for (idx, line) in tokenized_lines.into_iter().enumerate() {
            if idx < top_row || idx + overflow_lines >= top_row + height {
                continue;
            }

            let mut remaining_space = area.width as usize;
            let mut render_col_offset = 0;
            for token in line {
                match token {
                    CurlmanToken::Curl(text)
                    | CurlmanToken::Url(text)
                    | CurlmanToken::ParamKey(text)
                    | CurlmanToken::ParamValue(text)
                    | CurlmanToken::Whitespace(text)
                    | CurlmanToken::Separator(text)
                    | CurlmanToken::Unknown(text) => {
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

        self.editor.render(editor_area, buf);

        let EditorMode::Vim {
            ref state,
            ref mode,
        } = self.current_mode;
        const MOTION_BUFFER_RENDER_LEN: u16 = 16;

        let status_bar_str = if matches!(state.current_state, VimState::WritingCommand) {
            format!(":{}", &state.command_buff)
        } else {
            let number_of_spaces = command_bar_area
                .width
                .checked_sub(
                    state.repeat_n.len() as u16
                        + MOTION_BUFFER_RENDER_LEN
                        + mode.as_str().len() as u16
                        + 1,
                )
                .unwrap_or(0);

            format!(
                " {}{}{}{:?}",
                mode.as_str(),
                " ".repeat(number_of_spaces as usize),
                state.repeat_n,
                state.motion_buffer
            )
        };

        let status_bar_content = Line::from(status_bar_str).bold();
        status_bar_content.render(command_bar_area, buf);
    }
}

#[derive(Debug)]
pub enum WidgetCommand {
    Clear {},
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
            self.block = get_round_bordered_box()
                .border_style(Style::new().red())
                .title("Editor");
        } else {
            self.block = get_round_bordered_box().title("Editor");
        }
    }

    fn update_shared_state(&mut self, state: &AppState) -> Result<(), Error> {
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

        requests.render_ref(area, buf, &mut state.request_list_state);
    }
}

impl<'browser> InputListener for RequestBrowser<'browser> {
    fn handle_event(&mut self, e: Event) -> Option<WidgetCommand> {
        match e {
            Event::Key(key_event) => match key_event {
                KeyEvent {
                    code: KeyCode::Char(ch),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                } => return widget_common::move_widget_selection(ch),
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
                            let new_idx = *idx + 1;
                            if new_idx < requests.len() {
                                *idx = new_idx;
                                return Some(WidgetCommand::MoveRequestSelection { new_idx: *idx });
                            }
                        }
                        _ => {}
                    };
                }
                _ => {}
            },
            _ => {}
        };

        None
    }
}

impl<'browser> Default for RequestBrowser<'browser> {
    fn default() -> Self {
        RequestBrowser {
            block: get_round_bordered_box().title("Request Selector"),
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
            self.block = get_round_bordered_box()
                .border_style(Style::new().red())
                .title("Request Selector");
        } else {
            self.block = get_round_bordered_box().title("Request Selector");
        }
    }
    fn update_shared_state(&mut self, new_state: &AppState) -> Result<(), Error> {
        self.requests = Some(new_state.requests.clone());
        self.selected_request_idx = new_state.selected_request_idx;
        Ok(())
    }
}

pub fn get_round_bordered_box() -> Block<'static> {
    Block::bordered().border_type(BorderType::Rounded)
}
