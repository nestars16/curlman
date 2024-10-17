use std::char;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};

use gapbuf::{gap_buffer, GapBuffer};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    text::{Line, Text},
    widgets::{Block, BorderType, Paragraph, WidgetRef},
};

pub enum VimMode {
    Normal,
    Insert,
    Visual,
}

pub enum NormalModeInputTypes {
    Motion(VimCommand),
    Input(char),
}

pub enum VimState {
    AwaitingFirstInput,
    AwaitingOperatorOperand,
}

pub enum VimOperator {
    Up,
    Down,
    Left,
    Right,
    Until(char),
    Whole,
}

pub enum VimCommand {
    Move(VimOperator),
    InsertNewLineUp,
    InsertNewLineDown,
    Append,
    Insert,
    Delete(VimOperator),
    Yank(VimOperator),
}

impl TryFrom<char> for VimOperator {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'k' => Ok(Self::Up),
            'j' => Ok(Self::Down),
            'h' => Ok(Self::Left),
            'l' => Ok(Self::Right),
            'd' | 'y' => Ok(Self::Whole),
            _ => Err(()),
        }
    }
}

impl TryFrom<char> for VimCommand {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'i' => Ok(Self::Insert),
            'a' => Ok(Self::Append),
            'k' => Ok(Self::Move(VimOperator::Up)),
            'j' => Ok(Self::Move(VimOperator::Down)),
            'h' => Ok(Self::Move(VimOperator::Left)),
            'l' => Ok(Self::Move(VimOperator::Right)),
            'x' => Ok(Self::Delete(VimOperator::Right)),
            _ => Err(()),
        }
    }
}

impl TryFrom<[char; 3]> for VimCommand {
    type Error = ();

    fn try_from(value: [char; 3]) -> Result<Self, Self::Error> {
        match value {
            ['f', second, ' '] => Ok(Self::Move(VimOperator::Until(second))),
            ['d', 'd', ' '] => Ok(Self::Delete(VimOperator::Whole)),
            ['d', 'f', ' '] => Err(()),
            ['d', 'f', third] => Ok(Self::Delete(VimOperator::Until(third))),
            ['y', 'f', ' '] => Err(()),
            ['y', 'f', third] => Ok(Self::Yank(VimOperator::Until(third))),
            _ => Err(()),
        }
    }
}

pub struct VimEditorState {
    motion_buffer: [char; 3],
    repeat_n: String,
    current_state: VimState,
}

impl Default for VimEditorState {
    fn default() -> Self {
        VimEditorState {
            motion_buffer: [' ', ' ', ' '],
            current_state: VimState::AwaitingFirstInput,
            repeat_n: String::new(),
        }
    }
}

pub enum EditorMode {
    Vim {
        mode: VimMode,
        state: VimEditorState,
    },
    Regular,
}

pub struct Editor<'editor> {
    cursor: usize, //Its always pointing to the index in which a character would be inserted
    text_buffer: GapBuffer<char>,
    block: Block<'editor>,
    tab_len: u8,
    current_mode: EditorMode,
}

impl<'editor> Default for Editor<'editor> {
    fn default() -> Self {
        Editor {
            cursor: 0,
            text_buffer: gap_buffer![],
            block: get_round_bordered_box(),
            tab_len: 4,
            current_mode: EditorMode::Vim {
                mode: VimMode::Normal,
                state: VimEditorState {
                    repeat_n: String::new(),
                    motion_buffer: [' ', ' ', ' '],
                    current_state: VimState::AwaitingFirstInput,
                },
            },
        }
    }
}

impl<'editor> Editor<'editor> {
    fn get_editor_text(&self) -> Text {
        let mut char_buf = String::new();
        let mut spans = Vec::new();
        let mut lines = Vec::new();

        if self.text_buffer.is_empty() {
            return Text::styled(" ", Style::default().add_modifier(Modifier::REVERSED));
        }

        for (i, ch) in self.text_buffer.iter().enumerate() {
            if *ch == '\n' {
                if !char_buf.is_empty() {
                    spans.push(Span::from(std::mem::take(&mut char_buf)));
                }
                lines.push(Line::from(std::mem::take(&mut spans)));
                if i == self.cursor - 1 {
                    spans.push(Span::raw(" ").add_modifier(Modifier::REVERSED));
                }
            }

            if i == self.cursor - 1 {
                spans.push(Span::from(std::mem::take(&mut char_buf)));
                spans.push(Span::styled(
                    String::from(*ch),
                    Style::default().add_modifier(Modifier::REVERSED),
                ));
                continue;
            }

            if *ch == ' ' {
                char_buf.push(*ch);
                spans.push(Span::from(std::mem::take(&mut char_buf)));
                continue;
            }

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

    fn handle_input(&mut self, event: Event) {
        match event {
            Event::Key(key_event) => match self.current_mode {
                EditorMode::Vim { ref mode, .. } => match mode {
                    VimMode::Normal => self.handle_vim_motions(key_event),
                    VimMode::Insert => self.handle_key_events(key_event),
                    VimMode::Visual => todo!(),
                },
                EditorMode::Regular => self.handle_key_events(key_event),
            },
            Event::Mouse(mouse_event) => {}
            Event::Paste(_) => {}
            Event::FocusGained => {}
            Event::FocusLost => {}
            Event::Resize(_, _) => {}
        };
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
                let EditorMode::Vim { mode, .. } = &mut self.current_mode else {
                    return;
                };

                if !matches!(mode, VimMode::Insert) {
                    return;
                }

                self.switch_to_normal_mode()
            }
            _ => {}
        }
    }

    fn handle_commands(&mut self, command: VimCommand) {
        match command {
            VimCommand::Move(VimOperator::Up) => self.move_up(),
            VimCommand::Move(VimOperator::Down) => self.move_down(),
            VimCommand::Move(VimOperator::Left) => self.move_left(),
            VimCommand::Move(VimOperator::Right) => self.move_right(),
            VimCommand::Append => {
                self.switch_to_insert_mode();
            }
            VimCommand::Insert => {
                self.switch_to_insert_mode();
                if self.cursor > 0 {
                    self.cursor -= 1
                }
            }
            VimCommand::Delete(vim_operator) => match vim_operator {
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
                            stop = Some(i)
                        }
                    }

                    if let Some(stop) = stop {
                        self.text_buffer.drain((self.cursor - 1)..=stop);
                    }
                }
                VimOperator::Whole => {
                    self.text_buffer
                        .drain(self.get_line_start()..self.get_line_end());

                    if self.cursor > self.text_buffer.len() {
                        self.cursor = self.text_buffer.len()
                    }
                }
            },
            VimCommand::InsertNewLineDown => self.insert_newline(),
            _ => {}
        }
    }

    fn handle_vim_motions(&mut self, e: KeyEvent) {
        let mut command_to_exec = None;
        let EditorMode::Vim { ref mode, state } = &mut self.current_mode else {
            return;
        };

        if !matches!(mode, VimMode::Normal) {
            return;
        }

        match e {
            KeyEvent {
                code: KeyCode::Char(ch),
                ..
            } => match state.current_state {
                VimState::AwaitingFirstInput => {
                    if ch.is_ascii_digit() {
                        state.repeat_n.push(ch)
                    } else {
                        state.motion_buffer[0] = ch;
                        state.current_state = VimState::AwaitingOperatorOperand;
                        if let Ok(command) = TryInto::<VimCommand>::try_into(state.motion_buffer[0])
                        {
                            state.motion_buffer = [' ', ' ', ' '];
                            state.current_state = VimState::AwaitingFirstInput;
                            command_to_exec = Some(command);
                        };
                    }
                }
                VimState::AwaitingOperatorOperand => {
                    if state.motion_buffer[1] == ' ' {
                        state.motion_buffer[1] = ch
                    } else {
                        state.motion_buffer[2] = ch
                    }

                    match TryInto::<VimCommand>::try_into(state.motion_buffer) {
                        Ok(command) => {
                            state.current_state = VimState::AwaitingFirstInput;
                            state.motion_buffer = [' ', ' ', ' '];
                            command_to_exec = Some(command);
                        }
                        Err(_) => {
                            if state.motion_buffer[2] != ' ' {
                                state.current_state = VimState::AwaitingFirstInput;
                                state.motion_buffer = [' ', ' ', ' '];
                            }
                        }
                    }
                }
            },
            _ => {}
        };

        if let Some(command) = command_to_exec {
            self.handle_commands(command)
        }
    }

    fn switch_to_normal_mode(&mut self) {
        let EditorMode::Vim { mode, state } = &mut self.current_mode else {
            return;
        };

        *mode = VimMode::Normal;
        *state = VimEditorState::default();
    }

    fn switch_to_insert_mode(&mut self) {
        let EditorMode::Vim { mode, state } = &mut self.current_mode else {
            return;
        };

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
        let mut line_end_idx = self.text_buffer.len() - 1;

        for i in self.cursor..self.text_buffer.len() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                line_end_idx = i;
                break;
            }
        }

        line_end_idx
    }

    fn get_line_start(&self) -> usize {
        let mut line_start_idx = 0;

        for i in (0..=self.cursor).rev() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                line_start_idx = i as i32;
            }
        }

        line_start_idx as usize
    }

    fn move_up(&mut self) {
        let mut newline_count = 0;
        let mut prev_line_start_idx = 0;

        let mut line_start_idx = -1;

        for i in (0..=self.cursor).rev() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                match newline_count {
                    0 => line_start_idx = i as i32,
                    1 => prev_line_start_idx = i as i32,
                    _ => break,
                }
                newline_count += 1
            }
        }

        if line_start_idx == -1 {
            return;
        }

        let jump_idx = prev_line_start_idx as usize + (self.cursor - line_start_idx as usize);
        self.cursor = if jump_idx < line_start_idx as usize {
            jump_idx - 1
        } else {
            (line_start_idx) as usize
        };
    }

    fn move_down(&mut self) {
        let mut newline_count = 0;

        let (mut line_end_idx, mut next_line_end) = (-1, -1);
        for i in self.cursor..self.text_buffer.len() {
            if self.text_buffer.get(i).is_some_and(|ch| *ch == '\n') {
                match newline_count {
                    0 => line_end_idx = i as i32,
                    1 => next_line_end = i as i32,
                    _ => break,
                };
                newline_count += 1
            }
        }
        if line_end_idx == -1 {
            return;
        }

        if next_line_end == -1 {
            next_line_end = self.text_buffer.len() as i32 - 1;
        }

        let line_start_idx = self.get_line_start();

        let curr_offset = self.cursor - line_start_idx as usize;
        let next_line_length = next_line_end as usize - line_end_idx as usize;
        if curr_offset > next_line_length {
            self.cursor = self.text_buffer.len() - 1;
        } else {
            self.cursor = line_end_idx as usize + curr_offset + 1;
        }
    }

    fn move_right(&mut self) {
        if self.cursor >= self.text_buffer.len() {
            return;
        }

        self.cursor += 1
    }

    fn move_left(&mut self) {
        if self.cursor <= 1 {
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

    fn delete_char(&mut self) {
        if self.cursor < 1 {
            if !self.text_buffer.is_empty() {
                self.text_buffer.remove(0);
            }
            return;
        }
        self.text_buffer.remove(self.cursor - 1);
    }
}

impl<'editor> WidgetRef for Editor<'editor> {
    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let text_area = self.block.inner(area);
        let inner = Paragraph::new(self.get_editor_text());
        self.block.clone().render(area, buf);

        inner.render(text_area, buf);
    }
}

pub trait InputListener {
    fn handle_event(&mut self, e: Event);
}

pub trait CurlmanWidget: InputListener + WidgetRef {}

impl<'editor> InputListener for Editor<'editor> {
    fn handle_event(&mut self, e: Event) {
        self.handle_input(e)
    }
}

impl<'editor> CurlmanWidget for Editor<'editor> {}

pub struct RequestBrowser<'browser> {
    block: Block<'browser>,
}

impl<'browser> WidgetRef for RequestBrowser<'browser> {
    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        self.block.clone().render(area, buf)
    }
}

impl<'browser> InputListener for RequestBrowser<'browser> {
    fn handle_event(&mut self, e: Event) {}
}

impl<'browser> Default for RequestBrowser<'browser> {
    fn default() -> Self {
        RequestBrowser {
            block: get_round_bordered_box(),
        }
    }
}

impl<'browser> CurlmanWidget for RequestBrowser<'browser> {}

pub fn get_round_bordered_box() -> Block<'static> {
    Block::bordered().border_type(BorderType::Rounded)
}
