use crate::{
    editor::{self, CurlmanWidget, InputListener},
    error::Error,
    types::RequestInfo,
};
use curl::easy::{Easy, List, ReadError};
use http::Method;
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    text::Text,
    widgets::{Block, Widget, WidgetRef},
};
use std::io::Read;

pub struct RequestExecutor {
    output_data: Vec<u8>,
    block: Block<'static>,
}
impl Default for RequestExecutor {
    fn default() -> Self {
        Self {
            output_data: Vec::new(),
            block: editor::get_round_bordered_box(),
        }
    }
}

impl RequestExecutor {
    fn perform(&'static mut self, req: RequestInfo) -> Result<(), Error> {
        let mut handle = Easy::new();
        let Some(url) = req.url else {
            return Err(Error::InvalidUrl);
        };
        handle.url(&url.to_string())?;
        handle.write_function(move |data| {
            self.output_data.extend_from_slice(data);
            Ok(data.len())
        })?;
        let mut header_list = List::new();
        for (key, value) in req.headers {
            header_list.append(&format!("{key}: {value}"))?;
        }
        handle.http_headers(header_list)?;
        handle.timeout(req.timeout)?;
        match req.method {
            Method::GET => {
                handle.get(true)?;
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
            }
            _ => {}
        }
        handle.perform().map_err(|e| e.into())
    }
}

impl WidgetRef for RequestExecutor {
    #[doc = " Draws the current state of the widget in the given buffer. That is the only method required"]
    #[doc = " to implement a custom widget."]
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let text_area = self.block.inner(area);
        let inner = Text::from(String::from_utf8(self.output_data.clone()).unwrap());
        self.block.clone().render(area, buf);
        inner.render(text_area, buf);
    }
}

impl InputListener for RequestExecutor {
    fn handle_event(&mut self, e: crossterm::event::Event) {
        todo!()
    }
}

impl CurlmanWidget for RequestExecutor {}
