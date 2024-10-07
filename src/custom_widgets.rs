use std::char;

use crossterm::{
    cursor,
    event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers},
};
use gapbuf::{gap_buffer, GapBuffer};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    prelude::*,
    text::{Line, Text},
    widgets::{Block, BorderType, Paragraph, WidgetRef},
};

pub struct Editor<'editor> {
    cursor: usize,
    text_buffer: GapBuffer<char>,
    block: Block<'editor>,
    tab_len: u8,
    cursor_caret: char,
}

impl<'editor> Default for Editor<'editor> {
    fn default() -> Self {
        Editor {
            cursor_caret: '|',
            cursor: 0,
            text_buffer: gap_buffer![],
            block: get_round_bordered_box(),
            tab_len: 4,
        }
    }
}

impl<'editor> Editor<'editor> {
    fn get_editor_text(&self) -> Text {
        let mut char_buf = String::new();
        let mut spans = Vec::new();
        let mut lines = Vec::new();

        for (i, ch) in self.text_buffer.iter().enumerate() {
            if i == self.cursor && *ch != '\n' {
                char_buf.push(self.cursor_caret);
                continue;
            }

            if *ch == ' ' {
                char_buf.push(*ch);
                spans.push(Span::from(std::mem::take(&mut char_buf)));
                continue;
            }

            if *ch == '\n' {
                lines.push(Line::from(std::mem::take(&mut spans)));
                continue;
            }

            char_buf.push(*ch)
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
            Event::Key(key_event) => self.handle_key_events(key_event),
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
                modifiers: KeyModifiers::NONE,
                kind: KeyEventKind::Press | KeyEventKind::Repeat,
                state: KeyEventState::NONE,
            } => self.handle_char_key_presses(ch),
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => self.insert_newline(),
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => self.delete_char(),
            _ => {}
        }
    }

    fn handle_char_key_presses(&mut self, char: char) {
        match char {
            _ => self.insert_char(char),
        }
    }

    fn insert_char(&mut self, ch: char) {
        self.text_buffer.insert(self.cursor, ch);
        self.cursor += 1;
    }

    fn insert_newline(&mut self) {
        self.text_buffer.insert_many(self.cursor, [' ', '\n']);
        self.cursor += 2;
    }

    fn delete_char(&mut self) {
        if self.cursor <= 0 {
            return;
        }

        self.text_buffer
            .swap_remove(if self.cursor < self.text_buffer.len() {
                self.cursor
            } else {
                self.cursor - 1
            });

        self.cursor -= 1;
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
