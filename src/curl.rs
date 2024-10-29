use std::io::Read;

use crate::{
    editor::{self, CurlmanWidget, InputListener, WidgetCommand},
    error::Error,
    keys,
    types::RequestInfo,
    AppState,
};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use curl::easy::{Easy, List, ReadError};
use http::Method;
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Style, Stylize},
    text::Text,
    widgets::{Block, StatefulWidgetRef, Widget},
};

pub struct RequestExecutor {
    block: Block<'static>,
    selected: bool,
    cursor_idx: usize,
    output_data: Vec<u8>,
    request: Option<RequestInfo>,
}

impl RequestExecutor {
    pub fn new() -> Self {
        Self {
            block: editor::get_round_bordered_box(),
            selected: false,
            output_data: Vec::new(),
            request: None,
            cursor_idx: 0,
        }
    }

    pub fn perform_request(&mut self) -> Result<(), Error> {
        let mut handle = Easy::new();

        let Some(req) = self.request.clone() else {
            return Err(Error::InvalidState);
        };

        let Some(url) = req.url else {
            return Err(Error::InvalidUrl);
        };

        handle.url(url.as_ref())?;
        let mut header_list = List::new();

        for (key, value) in req.headers {
            header_list.append(&format!("{key}: {value}"))?;
        }
        handle.http_headers(header_list)?;
        handle.timeout(req.timeout)?;
        match req.method {
            Method::GET => {
                handle.get(true)?;

                let mut transfer = handle.transfer();

                transfer.write_function(move |data| {
                    self.output_data.extend_from_slice(data);
                    Ok(data.len())
                })?;

                transfer.perform().map_err(|e| e.into())
            }
            Method::POST => {
                handle.post(true)?;
                let Some(body) = req.body else {
                    return Err(Error::NoBody);
                };

                let mut transfer = handle.transfer();
                transfer.read_function(|into| match body.as_slice().read(into) {
                    Ok(read) => Ok(read),
                    Err(_) => Err(ReadError::Abort),
                })?;

                transfer.perform().map_err(|e| e.into())
            }
            _ => {
                unimplemented!()
            }
        }
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
        let text_area = self.block.inner(area);
        let inner = Text::from(String::from_utf8(self.output_data.clone()).unwrap());
        self.block.clone().render(area, buf);
        inner.render(text_area, buf);
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
                    if ch == 'E' {
                        self.output_data.clear();
                        match self.perform_request() {
                            Ok(_) => {}
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
