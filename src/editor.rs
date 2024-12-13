use crate::{error::Error, keys, types::RequestInfo, AppState};
use arboard::Clipboard;
use colors::{get_default_editor_colorscheme, EditorColorscheme};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    widgets::{Block, BorderType, List, StatefulWidgetRef},
};
use tui_textarea::{CursorMove, TextArea};

pub mod widget_common {
    use ratatui::{
        style::{Color, Stylize},
        text::{Line, Span},
    };

    use super::WidgetCommand;
    use crate::keys;

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

    /*
    pub fn get_next_cursor_position(
        current_row: usize,
        current_col: usize,
        lines: &Vec<String>,
        operator: CursorMovement,
    ) -> Option<(usize, usize)> {
        match operator {
            CursorMovement::Right if current_col >= lines[current_row].chars().count() => {
                (current_row + 1 < lines.len()).then(|| (current_row + 1, 0))
            }
            CursorMovement::Right => Some((current_row, current_col + 1)),
            CursorMovement::Left if current_col == 0 => {
                let row = current_row.checked_sub(1)?;
                Some((row, lines[row].chars().count()))
            }
            CursorMovement::Left => Some((current_row, current_col - 1)),
            CursorMovement::Up => {
                let row = current_row.checked_sub(1)?;
                Some((row, fit_col(current_col, &lines[row])))
            }
            CursorMovement::Down => Some((
                current_row + 1,
                fit_col(current_col, lines.get(current_row + 1)?),
            )),
            CursorMovement::UntilStartOfLine => Some((current_row, 0)),
            CursorMovement::UntilEndOfLine => {
                Some((current_row, lines[current_row].chars().count()))
            }
            CursorMovement::Until(_) => None,
            CursorMovement::Whole => None,
        }
    }

    fn split_at_utf8_safe(text: &str, split_char_idx: usize) -> (&str, &str) {
        text.split_at(get_split_byte_idx(text, split_char_idx))
    }

    fn get_split_byte_idx(text: &str, split_char_idx: usize) -> usize {
        let split_val_idx = text.char_indices().nth(split_char_idx);
        match split_val_idx {
            Some((split_val, _)) => split_val,
            None => 0,
        }
    }

    fn get_first_char_end(text: &str) -> usize {
        match text.char_indices().take(2).last() {
            Some((idx, _)) => idx,
            None => 1,
        }
    }

    pub fn fit_and_process_text_tokens_into_editor_window_utf8<'widget>(
        editor_col: usize,
        text: &'widget str,
        color: Color,
        area_width: u16,
        cursor_has_to_be_set: bool,
        is_cursor_set: &mut bool,
        current_col: &mut usize,
        lines: &mut Vec<Line<'widget>>,
        spans: &mut Vec<Span<'widget>>,
    ) {
        let will_overflow = *current_col + text.len() > area_width as usize;

        match (cursor_has_to_be_set, will_overflow) {
            (false, false) => {
                spans.push(Span::raw(text).fg(color));
            }
            (true, false) => {
                *is_cursor_set = true;
                //this is the index of the CHARACTER in which it will split
                let split_val_character_num = editor_col - *current_col;
                //this is the index of the BYTE of where the text will be split;
                let (before_cursor, at_and_before_cursor) =
                    split_at_utf8_safe(text, split_val_character_num);
                spans.push(Span::raw(before_cursor).fg(color));
                let first_character_end_idx = get_first_char_end(text);
                let string_segments = (
                    at_and_before_cursor.get(..first_character_end_idx),
                    at_and_before_cursor.get(first_character_end_idx..),
                );
                if let (Some(cursor_char), Some(rest_of_span)) = string_segments {
                    spans.push(Span::raw(cursor_char).fg(color).reversed());
                    spans.push(Span::raw(rest_of_span).fg(color));
                } else {
                    spans.push(Span::raw(" ").reversed());
                }
            }
            (false, true) => {
                let remaining_space_in_line =
                    (area_width as usize).checked_sub(*current_col).unwrap_or(0);
                let (start, mut end) = split_at_utf8_safe(text, remaining_space_in_line);
                spans.push(Span::raw(start).fg(color));
                lines.push(Line::from(std::mem::take(spans)));

                while end.len() > area_width as usize {
                    //FIXME make this tolerate utf8 bounds
                    spans.push(Span::raw(&end[..area_width as usize]).fg(color));
                    lines.push(Line::from(std::mem::take(spans)));
                    end = &end[area_width as usize..];
                }

                spans.push(Span::raw(end).fg(color));
                *current_col = end.len().checked_sub(1).unwrap_or(0);
            }
            (true, true) => {
                *is_cursor_set = true;

                let mut remaining_space_in_line =
                    (area_width as usize).checked_sub(*current_col).unwrap_or(0);

                let line_cursor_idx = editor_col.checked_sub(*current_col).unwrap_or(0);

                let (before_cursor, containing_and_after_cursor) =
                    split_at_utf8_safe(text, line_cursor_idx);

                match before_cursor.len() < remaining_space_in_line {
                    true => {
                        spans.push(Span::raw(before_cursor).fg(color));
                        remaining_space_in_line -= before_cursor.len();
                    }
                    false => {
                        //FIXME make this tolerate utf8 bounds
                        spans.push(Span::raw(&before_cursor[..remaining_space_in_line]).fg(color));
                        lines.push(Line::from(std::mem::take(spans)));
                        let mut remaining_before_cursor_text =
                            &before_cursor[remaining_space_in_line..];

                        while remaining_before_cursor_text.len() > area_width as usize {
                            spans.push(
                                Span::raw(&remaining_before_cursor_text[..area_width as usize])
                                    .fg(color),
                            );
                            lines.push(Line::from(std::mem::take(spans)));
                            remaining_before_cursor_text =
                                &remaining_before_cursor_text[area_width as usize..];
                        }

                        spans.push(Span::raw(remaining_before_cursor_text).fg(color));
                        remaining_space_in_line = (area_width as usize)
                            .checked_sub(remaining_before_cursor_text.len())
                            .unwrap_or(0);
                    }
                }
                match containing_and_after_cursor.len() {
                    0 => {}
                    1 => {
                        spans.push(Span::raw(containing_and_after_cursor).fg(color).reversed());
                    }
                    _ => {
                        let first_cursor_char_end = get_first_char_end(containing_and_after_cursor);
                        let cursor_char = &containing_and_after_cursor[..first_cursor_char_end];
                        assert!(cursor_char.len() == 1);
                        spans.push(Span::raw(cursor_char).fg(color).reversed());
                        remaining_space_in_line = match remaining_space_in_line.checked_sub(1) {
                            Some(new) => new,
                            None => {
                                lines.push(Line::from(std::mem::take(spans)));
                                area_width as usize
                            }
                        };
                        let mut after_cursor =
                            &containing_and_after_cursor[first_cursor_char_end..];

                        while after_cursor.len() > remaining_space_in_line {
                            //FIXME make this tolerate utf8 bounds
                            spans.push(
                                Span::raw(&after_cursor[..remaining_space_in_line]).fg(color),
                            );
                            lines.push(Line::from(std::mem::take(spans)));
                            after_cursor = &after_cursor[remaining_space_in_line..];

                            remaining_space_in_line = area_width as usize;
                        }
                        spans.push(Span::raw(after_cursor).fg(color));
                        *current_col = after_cursor.len();
                    }
                }
            }
        }
    }

    */

    pub fn fit_and_process_text_tokens_into_editor_window<'widget>(
        editor_col: usize,
        text: &'widget str,
        color: Color,
        area_width: u16,
        cursor_has_to_be_set: bool,
        is_cursor_set: &mut bool,
        current_col: &mut usize,
        lines: &mut Vec<Line<'widget>>,
        spans: &mut Vec<Span<'widget>>,
    ) {
        let will_overflow = *current_col + text.len() > area_width as usize;

        match (cursor_has_to_be_set, will_overflow) {
            (false, false) => {
                spans.push(Span::raw(text).fg(color));
            }
            (true, false) => {
                *is_cursor_set = true;

                let split_val = editor_col - *current_col;

                let (before_cursor, at_and_before_cursor) = text.split_at(split_val);

                spans.push(Span::raw(before_cursor).fg(color));

                let string_segments =
                    (at_and_before_cursor.get(..1), at_and_before_cursor.get(1..));

                if let (Some(cursor_char), Some(rest_of_span)) = string_segments {
                    spans.push(Span::raw(cursor_char).fg(color).reversed());
                    spans.push(Span::raw(rest_of_span).fg(color));
                } else {
                    spans.push(Span::raw(" ").reversed());
                }
            }
            (false, true) => {
                let remaining_space_in_line =
                    (area_width as usize).checked_sub(*current_col).unwrap_or(0);

                let (start, mut end) = text.split_at(remaining_space_in_line);

                spans.push(Span::raw(start).fg(color));
                lines.push(Line::from(std::mem::take(spans)));

                while end.len() > area_width as usize {
                    spans.push(Span::raw(&end[..area_width as usize]).fg(color));
                    lines.push(Line::from(std::mem::take(spans)));
                    end = &end[area_width as usize..];
                }

                spans.push(Span::raw(end).fg(color));
                *current_col = end.len().checked_sub(1).unwrap_or(0);
            }
            (true, true) => {
                *is_cursor_set = true;

                let mut remaining_space_in_line =
                    (area_width as usize).checked_sub(*current_col).unwrap_or(0);

                let line_cursor_idx = editor_col.checked_sub(*current_col).unwrap_or(0);

                let (before_cursor, containing_and_after_cursor) = text.split_at(line_cursor_idx);

                match before_cursor.len() < remaining_space_in_line {
                    true => {
                        spans.push(Span::raw(before_cursor).fg(color));
                        remaining_space_in_line -= before_cursor.len();
                    }
                    false => {
                        spans.push(Span::raw(&before_cursor[..remaining_space_in_line]).fg(color));
                        lines.push(Line::from(std::mem::take(spans)));
                        let mut remaining_before_cursor_text =
                            &before_cursor[remaining_space_in_line..];

                        while remaining_before_cursor_text.len() > area_width as usize {
                            spans.push(
                                Span::raw(&remaining_before_cursor_text[..area_width as usize])
                                    .fg(color),
                            );
                            lines.push(Line::from(std::mem::take(spans)));
                            remaining_before_cursor_text =
                                &remaining_before_cursor_text[area_width as usize..];
                        }

                        spans.push(Span::raw(remaining_before_cursor_text).fg(color));

                        remaining_space_in_line = (area_width as usize)
                            .checked_sub(remaining_before_cursor_text.len())
                            .unwrap_or(0);
                    }
                }
                match containing_and_after_cursor.len() {
                    0 => {}
                    1 => {
                        spans.push(Span::raw(containing_and_after_cursor).fg(color).reversed());
                    }
                    _ => {
                        let cursor_char = &containing_and_after_cursor[..1];
                        assert!(cursor_char.len() == 1);
                        spans.push(Span::raw(cursor_char).fg(color).reversed());
                        remaining_space_in_line = match remaining_space_in_line.checked_sub(1) {
                            Some(new) => new,
                            None => {
                                lines.push(Line::from(std::mem::take(spans)));
                                area_width as usize
                            }
                        };
                        let mut after_cursor = &containing_and_after_cursor[1..];
                        while after_cursor.len() > remaining_space_in_line {
                            spans.push(
                                Span::raw(&after_cursor[..remaining_space_in_line]).fg(color),
                            );

                            lines.push(Line::from(std::mem::take(spans)));
                            after_cursor = &after_cursor[remaining_space_in_line..];

                            remaining_space_in_line = area_width as usize;
                        }
                        spans.push(Span::raw(after_cursor).fg(color));
                        *current_col = after_cursor.len();
                    }
                }
            }
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

#[derive(Debug, Clone)]
pub enum CursorMovement {
    Regular(CursorMove),
    Until(char),
    WholeLine,
}

impl TryFrom<CursorMovement> for CursorMove {
    type Error = ();

    fn try_from(value: CursorMovement) -> Result<Self, Self::Error> {
        match value {
            CursorMovement::Regular(cursor_move) => Ok(cursor_move),
            CursorMovement::Until(_) => Err(()),
            CursorMovement::WholeLine => Err(()),
        }
    }
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
            'k' => Ok(Self::Regular(CursorMove::Up)),
            'j' => Ok(Self::Regular(CursorMove::Down)),
            '$' => Ok(Self::Regular(CursorMove::End)),
            '0' => Ok(Self::Regular(CursorMove::Head)),
            'h' => Ok(Self::Regular(CursorMove::Back)),
            'l' => Ok(Self::Regular(CursorMove::Forward)),
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
            'k' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Up))),
            'j' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Down))),
            'h' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Back))),
            'l' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Forward))),
            'o' => Ok(Self::InsertNewLineDown),
            '0' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Head))),
            'G' => Ok(Self::Move(CursorMovement::Regular(CursorMove::Bottom))),
            '$' => Ok(Self::Move(CursorMovement::Regular(CursorMove::End))),
            'x' => Ok(Self::Delete(CursorMovement::Regular(CursorMove::Back))),
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
            ['g', 'g', ' '] => Ok(Self::Move(CursorMovement::Regular(CursorMove::Top))),
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
    pub editor: TextArea<'editor>,
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
            editor: TextArea::from(start_state),
        }
    }

    /*
    fn get_editor_text(&self, area: Rect) -> Text {
        let tokenized_lines = parse_curlman_editor(&self.lines);
        let mut spans = Vec::new();
        let mut lines = Vec::new();
        let mut is_cursor_set = false;

        for (row_idx, line) in tokenized_lines.into_iter().enumerate() {
            if row_idx < self.top_row as usize
                || row_idx >= self.top_row as usize + area.height as usize
            {
                continue;
            }

            let mut current_col = 0;
            for line_token in line {
                let line_token_len = line_token.get_str().len();
                let line_token_end = (current_col + line_token_len).checked_sub(1).unwrap_or(0);

                let cursor_has_to_be_set =
                    !is_cursor_set && row_idx == self.row && line_token_end >= self.col;

                let color = line_token.get_color(&self.colorscheme);

                match line_token {
                    crate::parser::Token::Curl(text)
                    | crate::parser::Token::Url(text)
                    | crate::parser::Token::ParamKey(text)
                    | crate::parser::Token::ParamValue(text)
                    | crate::parser::Token::Unknown(text)
                    | crate::parser::Token::Separator(text) => {
                        widget_common::fit_and_process_text_tokens_into_editor_window(
                            self.col,
                            text,
                            color,
                            area.width,
                            cursor_has_to_be_set,
                            &mut is_cursor_set,
                            &mut current_col,
                            &mut lines,
                            &mut spans,
                        )
                    }
                    crate::parser::Token::Whitespace(text) => {
                        widget_common::fit_and_process_text_tokens_into_editor_window(
                            self.col,
                            text,
                            Color::White,
                            area.width,
                            cursor_has_to_be_set,
                            &mut is_cursor_set,
                            &mut current_col,
                            &mut lines,
                            &mut spans,
                        )
                    }
                };

                current_col += line_token_len;
            }

            if row_idx == self.row && self.col == self.lines[self.row].len() {
                if self.col as u16 == area.width {
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
    */

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
            } => self.editor.move_cursor(CursorMove::Up),
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMove::Down),
            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMove::Back),
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.editor.move_cursor(CursorMove::Forward),
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
                self.move_cursor(CursorMovement::Regular(CursorMove::End));
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
