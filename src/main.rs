use std::io::{BufWriter, Read, Seek, SeekFrom, Write};
mod curl;
mod editor;
mod error;
mod parser;
mod types;

use crossterm::terminal::enable_raw_mode;
use crossterm::{
    event::{self},
    terminal::disable_raw_mode,
};
use editor::WidgetCommand;
use nom_locate::LocatedSpan;
use parser::parse_curlman_request_file;
use ratatui::widgets::ListState;
use ratatui::{layout::Layout, prelude::*, DefaultTerminal};
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use types::{DirectionArray, LayoutParent, Pane, PaneWidget, RequestInfo, TargetId};

pub struct AppState {
    pub list_state: ListState,
    pub requests: Vec<RequestInfo>,
    pub selected_request_idx: Option<usize>,
}

struct App {
    request_file: File,
    state: AppState,
    panes: HashMap<u32, Pane>,
    layouts: HashMap<u32, Layout>,
    selected_pane_id: u32,
    selected_widget_idx: usize,
}

//TODO
//Add input to request browser
//sync state across widgets
//json parsing and output
//finishing vim keybinds
//syntax highlighting (json editor)
//correct error handling and ui

pub mod keys {
    pub const UP: char = 'k';
    pub const DOWN: char = 'j';
    pub const LEFT: char = 'h';
    pub const RIGHT: char = 'l';

    pub enum Direction {
        Up,
        Down,
        Left,
        Right,
    }

    impl From<Direction> for usize {
        fn from(value: Direction) -> Self {
            match value {
                Direction::Up => 0,
                Direction::Down => 1,
                Direction::Left => 2,
                Direction::Right => 3,
            }
        }
    }
}

impl App {
    fn new(
        panes: Vec<Pane>,
        layouts: Vec<Layout>,
        default_pane_id: u32,
        default_widget_idx: usize,
        file: File,
        selected_request_idx: Option<usize>,
    ) -> Self {
        assert_eq!(panes.len(), layouts.len());

        let pane_map_iter = panes
            .into_iter()
            .enumerate()
            .map(|(idx, pane)| (idx as u32, pane));

        let layout_map_iter = layouts
            .into_iter()
            .enumerate()
            .map(|(idx, layout)| (idx as u32, layout));

        App {
            panes: HashMap::from_iter(pane_map_iter),
            layouts: HashMap::from_iter(layout_map_iter),
            selected_pane_id: default_pane_id,
            selected_widget_idx: default_widget_idx,
            state: AppState {
                requests: Vec::new(),
                list_state: ListState::default().with_selected(selected_request_idx),
                selected_request_idx,
            },
            request_file: file,
        }
    }

    fn save_to_request_file_and_update_widgets(
        &mut self,
        new_text: &str,
    ) -> Result<(), crate::error::Error> {
        self.save_into_request_file(&new_text)?;

        let requests = match parse_curlman_request_file(LocatedSpan::<&str>::new(&new_text)) {
            Ok((_, vec)) => vec,
            Err(_) => Vec::new(),
        };
        let state = AppState {
            list_state: self.state.list_state.clone(),
            selected_request_idx: self.state.selected_request_idx.clone(),
            requests,
        };

        for (_, pane) in &mut self.panes {
            for widget in &mut pane.widgets {
                widget.widget.update_shared_state(&state)?;
            }
        }
        Ok(())
    }

    fn save_into_request_file(&mut self, new_text: &str) -> Result<(), crate::error::Error> {
        self.request_file.seek(SeekFrom::Start(0))?;
        BufWriter::new(&self.request_file).write_all(new_text.as_bytes())?;
        self.request_file.set_len(new_text.len() as u64)?;
        Ok(())
    }

    fn run(&mut self, term: &mut DefaultTerminal) -> Result<(), crate::error::Error> {
        loop {
            term.draw(|frame| {
                for (_, pane) in self.panes.iter() {
                    let widget_layout = if let Some(LayoutParent {
                        layout_idx,
                        layout_pos_idx,
                    }) = pane.layout_parent
                    {
                        let parent_layout = self
                            .layouts
                            .get(&layout_idx)
                            .expect("LAYOUT IDX MUST EXIST");

                        self.layouts[&pane.layout_id]
                            .split(parent_layout.split(frame.area())[layout_pos_idx])
                    } else {
                        self.layouts[&pane.layout_id].split(frame.area())
                    };

                    for PaneWidget {
                        widget,
                        layout_idx: layout_id,
                        ..
                    } in pane.widgets.iter()
                    {
                        widget.render_ref(
                            widget_layout[*layout_id],
                            frame.buffer_mut(),
                            &mut self.state,
                        )
                    }
                }
            })?;

            let event = event::read()?;
            let selected_pane = self
                .panes
                .get_mut(&self.selected_pane_id)
                .expect("Id not in panes");

            let selected_pane_widget = selected_pane
                .widgets
                .get_mut(self.selected_widget_idx)
                .expect("Id not in widgets");

            let selected_pane_widget_inner = &mut selected_pane_widget.widget;
            if let Some(cmd) = selected_pane_widget_inner.handle_event(event) {
                match cmd {
                    WidgetCommand::MoveSelection { direction } => {
                        selected_pane_widget_inner.toggle_selected();

                        if let Some(new_widget_idx) =
                            selected_pane.get_next_widget_idx(self.selected_widget_idx, direction)
                        {
                            self.selected_widget_idx = new_widget_idx;
                        }

                        selected_pane.widgets[self.selected_widget_idx]
                            .widget
                            .toggle_selected();
                    }
                    WidgetCommand::Save { text } => {
                        self.save_to_request_file_and_update_widgets(&text)?;
                    }
                    WidgetCommand::MoveRequest {
                        new_idx,
                        old_request_buffer,
                    } => {
                        self.save_to_request_file_and_update_widgets(&old_request_buffer)?;
                        self.selected_widget_idx = new_idx;
                    }
                    WidgetCommand::Quit => {
                        return Ok(());
                    }
                }
            };
        }
    }
}

fn main() -> Result<(), crate::error::Error> {
    let mut terminal = ratatui::init();
    terminal.clear()?;
    enable_raw_mode()?;
    let mut raw_buffer = String::new();

    let (curlman_file, buffer) = match OpenOptions::new().read(true).write(true).open("./.curlman")
    {
        Ok(mut file) => {
            file.read_to_string(&mut raw_buffer)?;
            (file, Some(raw_buffer))
        }
        Err(_) => (File::create_new("./.curlman")?, None),
    };

    let editor_widget = Box::new(editor::Editor::new(
        buffer.clone().map(|e| e.chars().collect()),
    ));
    let output_widget = Box::new(curl::RequestExecutor::new());
    let mut initally_selected_request = None;

    let request_browser_widget = Box::new(if let Some(buffer) = buffer {
        match parse_curlman_request_file(LocatedSpan::<&str>::new(&buffer)) {
            Ok((_, vec)) => {
                let browser = editor::RequestBrowser::from(vec);
                initally_selected_request = browser.selected_request_idx;
                browser
            }
            Err(_) => editor::RequestBrowser::default(),
        }
    } else {
        editor::RequestBrowser::default()
    });

    let parent_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(25), Constraint::Percentage(75)]);
    let inner_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(40), Constraint::Percentage(60)]);

    let left_side_widgets = vec![PaneWidget::new(
        request_browser_widget,
        1,
        DirectionArray::NONE,
    )];

    let right_side_widgets = vec![
        PaneWidget::new(
            editor_widget,
            0,
            DirectionArray([None, Some(TargetId(1)), None, None]),
        ),
        PaneWidget::new(
            output_widget,
            1,
            DirectionArray([Some(TargetId(0)), None, None, None]),
        ),
    ];

    let layouts = vec![parent_layout, inner_layout];
    let panes = vec![
        Pane::new(left_side_widgets, Some(LayoutParent::new(0, 0)), 0),
        Pane::new(right_side_widgets, Some(LayoutParent::new(0, 1)), 1),
    ];

    let mut app = App::new(
        panes,
        layouts,
        1,
        0,
        curlman_file,
        initally_selected_request,
    );
    app.run(&mut terminal)?;
    ratatui::restore();
    disable_raw_mode()?;
    Ok(())
}
