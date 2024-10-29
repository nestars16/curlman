use crate::{error::Error, parser::parse_curlman_request_file};

//TODO?
//Add vertical scrolling of text?
//timeout for motions?

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};

use gapbuf::GapBuffer;
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    text::{Line, Text},
    widgets::{Block, BorderType, List, Paragraph, StatefulWidgetRef},
};

use crate::{keys, types::RequestInfo, AppState};

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
pub enum VimOperator {
    Up,
    Down,
    Left,
    Right,
    Until(char),
    Whole,
    UntilEnd,
    UntilStart,
}

#[derive(Debug)]
pub enum VimMotion {
    Move(VimOperator),
    InsertNewLineDown,
    Append,
    Insert,
    Delete(VimOperator),
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

impl TryFrom<char> for VimOperator {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'k' => Ok(Self::Up),
            'j' => Ok(Self::Down),
            '$' => Ok(Self::UntilEnd),
            '0' => Ok(Self::UntilStart),
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
            'k' => Ok(Self::Move(VimOperator::Up)),
            'j' => Ok(Self::Move(VimOperator::Down)),
            'h' => Ok(Self::Move(VimOperator::Left)),
            'l' => Ok(Self::Move(VimOperator::Right)),
            'o' => Ok(Self::InsertNewLineDown),
            '0' => Ok(Self::Move(VimOperator::UntilStart)),
            '$' => Ok(Self::Move(VimOperator::UntilEnd)),
            'x' => Ok(Self::Delete(VimOperator::Right)),
            _ => Err(()),
        }
    }
}

impl TryFrom<[char; 3]> for VimMotion {
    type Error = ();

    fn try_from(value: [char; 3]) -> Result<Self, Self::Error> {
        match value {
            ['f', second, ' '] => Ok(Self::Move(VimOperator::Until(second))),
            ['d', 'd', ' '] => Ok(Self::Delete(VimOperator::Whole)),
            ['d', 'f', ' '] => Err(()),
            ['d', 'f', third] => Ok(Self::Delete(VimOperator::Until(third))),
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
    cursor: usize, //Its always pointing to the index in which a character would be inserted
    pub text_buffer: GapBuffer<char>,
    current_editor_str: String,
    block: Block<'editor>,
    tab_len: u8,
    current_mode: EditorMode,
    selected: bool,
}

impl<'editor> Editor<'editor> {
    pub fn new(text_buffer: Option<GapBuffer<char>>) -> Self {
        Self {
            cursor: 0,
            text_buffer: text_buffer.unwrap_or(GapBuffer::new()),
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
            current_editor_str: String::new(),
        }
    }

    fn get_editor_text_syntax(&self, area: Rect) -> Text {
        let EditorMode::Vim { ref state, .. } = self.current_mode;
        let cursor_condition =
            !matches!(state.current_state, VimState::WritingCommand) && self.selected;
        if self.text_buffer.is_empty() && cursor_condition {
            return Text::styled(" ", Style::default().add_modifier(Modifier::REVERSED));
        }
        let cursor_value = if self.cursor > 0 {
            self.cursor - 1
        } else {
            self.cursor
        };
        let mut col_counter = 0;

        let (_, (_, tokens)) = parse_curlman_request_file(&self.current_editor_str).unwrap();

        let mut char_buf = String::new();
        let mut spans = Vec::new();
        let mut lines = Vec::new();
        for token in tokens {
            match token {
                crate::parser::Token::Curl(text) => {
                    spans.push(Span::raw(text).cyan());
                }
                crate::parser::Token::Url(text) => {
                    spans.push(Span::raw(text).yellow());
                }
                crate::parser::Token::ParamKey(text) => {
                    spans.push(Span::raw(text).green());
                }
                crate::parser::Token::ParamValue(text) => {
                    spans.push(Span::raw(text).blue());
                }
                crate::parser::Token::Whitespace(text) => {
                    for ch in text.chars() {
                        if ch == '\n' {
                            spans.push(Span::raw(std::mem::take(&mut char_buf)));
                            lines.push(Line::from(std::mem::take(&mut spans)));
                            continue;
                        }

                        char_buf.push(ch);
                    }

                    if !char_buf.is_empty() {
                        spans.push(Span::raw(std::mem::take(&mut char_buf)));
                        lines.push(Line::from(std::mem::take(&mut spans)));
                    }
                }
            };
        }

        Text::from(lines)
    }

    fn get_editor_text(&self, area: Rect) -> Text {
        let EditorMode::Vim { ref state, .. } = self.current_mode;

        let mut char_buf = String::new();
        let mut spans = Vec::new();
        let mut lines = Vec::new();

        let cursor_condition =
            !matches!(state.current_state, VimState::WritingCommand) && self.selected;

        if self.text_buffer.is_empty() && cursor_condition {
            return Text::styled(" ", Style::default().add_modifier(Modifier::REVERSED));
        }

        let cursor_value = if self.cursor > 0 {
            self.cursor - 1
        } else {
            self.cursor
        };

        let mut col_counter = 0;

        for (i, ch) in self.text_buffer.iter().enumerate() {
            if *ch == '\n' || col_counter > area.width {
                col_counter = 0;

                if !char_buf.is_empty() {
                    spans.push(Span::from(std::mem::take(&mut char_buf)));
                }

                lines.push(Line::from(std::mem::take(&mut spans)));

                if i == cursor_value && *ch == '\n' && cursor_condition {
                    spans.push(Span::raw(" ").add_modifier(Modifier::REVERSED));
                }
            }

            if i == cursor_value && cursor_condition {
                col_counter += 1;
                spans.push(Span::from(std::mem::take(&mut char_buf)));

                spans.push(Span::styled(
                    String::from(*ch),
                    Style::default().add_modifier(Modifier::REVERSED),
                ));
                continue;
            }

            if *ch == ' ' {
                col_counter += 1;
                char_buf.push(*ch);
                spans.push(Span::from(std::mem::take(&mut char_buf)));
                continue;
            }

            col_counter += 1;
            char_buf.push(*ch);
        }

        if !char_buf.is_empty() {
            spans.push(Span::from(std::mem::take(&mut char_buf)));
        }

        if !spans.is_empty() {
            lines.push(Line::from(std::mem::take(&mut spans)));
        }

        Text::from(lines)
    }

    fn handle_input(&mut self, event: Event) -> Option<WidgetCommand> {
        match event {
            Event::Key(key_event) => match self.current_mode {
                EditorMode::Vim { ref mode, .. } => match mode {
                    VimMode::Normal => return self.handle_vim_motions(key_event),
                    VimMode::Insert => self.handle_key_events(key_event),
                },
            },
            Event::Paste(e) => {
                self.text_buffer.insert_many(self.cursor, e.chars());
            }
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
            } => self.insert_char(ch),
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
            } => self.move_up(),
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_down(),
            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_left(),
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::NONE,
                ..
            } => self.move_right(),
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
                    text: self.text_buffer.clone().into_iter().collect::<String>(),
                });
            }
            VimCommand::Quit => {
                return Some(WidgetCommand::Quit);
            }
        }
    }

    fn handle_motions(&mut self, motion: VimMotion) {
        match motion {
            VimMotion::Move(VimOperator::Up) => self.move_up(),
            VimMotion::Move(VimOperator::Down) => self.move_down(),
            VimMotion::Move(VimOperator::Left) => self.move_left(),
            VimMotion::Move(VimOperator::Right) => self.move_right(),
            VimMotion::Move(VimOperator::UntilEnd) => {
                self.cursor = self.get_line_end();
            }
            VimMotion::Move(VimOperator::UntilStart) => {
                self.cursor = self.get_line_start();
            }
            VimMotion::Append => {
                self.switch_to_insert_mode();
            }
            VimMotion::Insert => {
                self.switch_to_insert_mode();
                if self.cursor > 0 {
                    self.cursor -= 1
                }
            }
            VimMotion::Delete(vim_operator) => match vim_operator {
                VimOperator::Up => {}
                VimOperator::Down => {}
                VimOperator::Left => {
                    if self.cursor > 2 {
                        self.text_buffer.remove(self.cursor - 2);
                        self.cursor -= 1;
                    }
                }
                VimOperator::Right => {
                    self.backspace_delete();
                }
                VimOperator::Until(char) => {
                    let mut stop: Option<usize> = None;

                    let line_end_idx = self.get_line_end();

                    for i in self.cursor..=line_end_idx {
                        if self.text_buffer.get(i).is_some_and(|ch| *ch == char) {
                            stop = Some(i);
                            break;
                        }
                    }

                    if let Some(stop) = stop {
                        self.text_buffer.drain((self.cursor - 1)..=stop);
                    }
                }
                VimOperator::Whole => {}
                VimOperator::UntilEnd => {
                    self.text_buffer.drain(self.cursor..self.get_line_end());

                    if self.cursor > self.text_buffer.len() {
                        self.cursor = self.text_buffer.len()
                    }
                }
                VimOperator::UntilStart => {
                    self.text_buffer
                        .drain(self.get_line_start() + 1..self.cursor);

                    if self.cursor > self.text_buffer.len() {
                        self.cursor = self.text_buffer.len()
                    }
                }
            },
            VimMotion::InsertNewLineDown => {
                self.cursor = self.text_buffer.len();
                self.insert_char('\n');
            }
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
        self.text_buffer.insert(self.cursor, ch);
        self.cursor += 1;
    }

    fn insert_newline(&mut self) {
        self.text_buffer.insert(self.cursor, '\n');
        self.cursor += 1;
    }

    fn insert_tab(&mut self) {
        self.text_buffer
            .insert_many(self.cursor, (0..self.tab_len).map(|_| ' '));

        self.cursor += self.tab_len as usize;
    }

    fn get_line_end(&self) -> usize {
        let mut line_end_idx = None;
        let buffer_len = self.text_buffer.len();

        for i in self.cursor..buffer_len {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                line_end_idx = Some(i);
                break;
            }
        }

        line_end_idx.unwrap_or(buffer_len)
    }

    fn get_line_start(&self) -> usize {
        let mut line_start_idx = 0;
        for i in (0..self.cursor).rev() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                line_start_idx = i as i32;
                break;
            }
        }

        line_start_idx as usize
    }

    fn move_up(&mut self) {
        let line_start = self.get_line_start();

        if line_start == 0 {
            return;
        }

        // Find the start of the previous line
        let mut prev_line_start = None;

        for i in (0..line_start).rev() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                prev_line_start = Some(i);
                break;
            }
        }

        let curr_offset = self.cursor - line_start;

        let Some(prev_line_start) = prev_line_start else {
            self.cursor = if curr_offset < line_start {
                curr_offset - 1
            } else {
                line_start
            };

            return;
        };

        //The idea here is similar to our move down after finding our line_start
        //and our previous_line_start, since we know our offset
        //is self.cursor - line_start all we have to do is
        //set our self.cursor to previous_line_start + offset
        //if the line is equal or larger to the offset, otherwise
        // we set it to previous_line_start + 1
        let target = prev_line_start + curr_offset;

        self.cursor = if target >= line_start {
            line_start
        } else {
            target
        }
    }

    fn move_down(&mut self) {
        let mut line_end: Option<usize> = None;
        let mut next_line_end: Option<usize> = None;
        let mut newline_count = 0;

        for i in self.cursor..self.text_buffer.len() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                match newline_count {
                    0 => {
                        line_end = Some(i);
                        newline_count += 1;
                    }
                    1 => next_line_end = Some(i),
                    _ => {
                        unreachable!()
                    }
                }
            }
        }

        //This means that we found newline from
        //our cursor onwards, meaning nowhere to go
        let Some(line_end) = line_end else {
            return;
        };

        let line_start = self.get_line_start();
        let curr_offset = self.cursor - line_start;
        let target = line_end + curr_offset;

        //Here we check if needs to bounds check, if there is no
        //next line after our line end then we can just
        //check our current line offset and see if it fits in the next line
        //if it doesn't then we just set it at the end of the line
        let Some(next_line_end) = next_line_end else {
            self.cursor = if target < self.text_buffer.len() {
                if line_start == 0 {
                    target + 1
                } else {
                    target
                }
            } else {
                self.text_buffer.len()
            };
            return;
        };

        self.cursor = if target > next_line_end {
            next_line_end
        } else {
            target + if line_start == 0 { 1 } else { 0 }
        }
    }

    fn move_right(&mut self) {
        if self.cursor >= self.text_buffer.len()
            || self
                .text_buffer
                .get(self.cursor)
                .is_some_and(|ch| *ch == '\n')
        {
            return;
        };

        self.cursor += 1
    }

    fn move_left(&mut self) {
        if self.cursor <= 1
            || self
                .text_buffer
                .get(self.cursor - 1)
                .is_some_and(|ch| *ch == '\n')
        {
            return;
        }

        self.cursor -= 1;
    }

    fn backspace_delete(&mut self) {
        if self.cursor < 1 {
            if !self.text_buffer.is_empty() {
                self.text_buffer.remove(0);
            }
            return;
        }

        self.text_buffer.remove(self.cursor - 1);
        self.cursor -= 1;
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
