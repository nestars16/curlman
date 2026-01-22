use std::io::{BufWriter, Read, Seek, SeekFrom, Write};
mod cursor_movements;
mod editor;
mod error;
mod executor;
mod new_parser;
mod parser;
mod types;

use crossterm::terminal::enable_raw_mode;
use crossterm::{
    event::{self},
    terminal::disable_raw_mode,
};

use editor::WidgetCommand;
use jq_sys::{jq_init, jq_state, jq_teardown};
use parser::parse_curlman_request_file;

use ratatui::{
    layout::{Direction, Layout},
    prelude::*,
    widgets::ListState,
    DefaultTerminal,
};

use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use types::{DirectionArray, LayoutParent, Pane, PaneWidget, RequestInfo, TargetId};

#[derive(Debug)]
pub struct AppState {
    pub request_list_state: ListState,
    pub requests: Vec<RequestInfo>,
    pub selected_request_idx: Option<usize>,
}

struct App {
    previous_widget_indexes: Option<(u32, usize)>,
    request_file: File,
    state: AppState,
    panes: HashMap<u32, Pane>,
    layouts: HashMap<u32, Layout>,
    selected_pane_id: u32,
    selected_widget_idx: usize,
    jq_handle: *mut jq_state,
}

impl Drop for App {
    fn drop(&mut self) {
        if !self.jq_handle.is_null() {
            unsafe { jq_teardown(&mut self.jq_handle) }
        }
        ratatui::restore();
    }
}

//TODO
//FIX ADJUSTING VIEWPORT
//Add yanking and visual mode to vim and complete deletes
//Add jq json filtering
//curl file uploads, basic auth and auth redirect

pub mod keys {
    pub const UP: char = 'k';
    pub const DOWN: char = 'j';
    pub const LEFT: char = 'h';
    pub const RIGHT: char = 'l';

    #[derive(Clone, Debug)]
    pub enum Direction {
        Up,
        Down,
        Left,
        Right,
    }

    impl TryFrom<char> for Direction {
        type Error = ();

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                'j' => Ok(Self::Down),
                'k' => Ok(Self::Up),
                'h' => Ok(Self::Left),
                'l' => Ok(Self::Right),
                _ => Err(()),
            }
        }
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
        state: AppState,
        jq_handle: *mut jq_state,
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
            state,
            request_file: file,
            previous_widget_indexes: None,
            jq_handle,
        }
    }

    fn save_to_request_file_and_update_widgets(
        &mut self,
        new_text: &str,
    ) -> Result<(), crate::error::Error> {
        self.save_into_request_file(&new_text)?;

        let requests = match parse_curlman_request_file(&new_text) {
            Ok((_, parsed_requests)) => parsed_requests,
            Err(_) => Vec::new(),
        };

        match self.state.selected_request_idx {
            Some(idx) => {
                if requests.is_empty() {
                    self.state.selected_request_idx = None;
                } else if idx > requests.len() - 1 {
                    self.state.selected_request_idx = Some(0);
                }
            }
            None => {
                if requests.is_empty() {
                    self.state.selected_request_idx = None;
                } else {
                    self.state.selected_request_idx = Some(0);
                }
            }
        }

        self.state.requests = requests;
        self.state
            .request_list_state
            .select(self.state.selected_request_idx);

        for (_, pane) in &mut self.panes {
            for widget in &mut pane.widgets {
                widget.widget.update_shared_state(&self.state)?;
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

    fn get_next_pane_id(&self, direction: keys::Direction) -> Option<u32> {
        let pane_in_direction =
            &self.panes[&self.selected_pane_id].available_directions.0[direction as usize];

        match pane_in_direction {
            Some(TargetId(id)) => Some(*id as u32),
            None => None,
        }
    }

    fn run(&mut self, term: &mut DefaultTerminal) -> Result<(), crate::error::Error> {
        loop {
            if let Some((pane_id, widget_idx)) = self.previous_widget_indexes {
                if let Some(pane) = self.panes.get_mut(&pane_id) {
                    pane.widgets[widget_idx].widget.toggle_selected();
                }
                self.previous_widget_indexes = None;
            }

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
                    WidgetCommand::MoveWidgetSelection { direction } => {
                        match selected_pane
                            .get_next_widget_idx(self.selected_widget_idx, direction.clone())
                        {
                            Some(new_widget_idx) => {
                                self.previous_widget_indexes =
                                    Some((self.selected_pane_id, self.selected_widget_idx));

                                self.selected_widget_idx = new_widget_idx;
                                selected_pane.widgets[self.selected_widget_idx]
                                    .widget
                                    .toggle_selected();
                            }
                            None => {
                                if let Some(id) = self.get_next_pane_id(direction) {
                                    self.previous_widget_indexes =
                                        Some((self.selected_pane_id, self.selected_widget_idx));

                                    self.selected_pane_id = id;
                                    self.selected_widget_idx = 0;

                                    if self
                                        .previous_widget_indexes
                                        .is_some_and(|(previous_pane_id, _)| previous_pane_id == id)
                                    {
                                        self.selected_widget_idx = self
                                            .previous_widget_indexes
                                            .expect("Guaranteed to be some")
                                            .1;
                                    }

                                    let new_selected_pane = self
                                        .panes
                                        .get_mut(&self.selected_pane_id)
                                        .expect("Id not in panes");

                                    let _ = new_selected_pane
                                        .widgets
                                        .get_mut(self.selected_widget_idx)
                                        .expect("Id not in widgets")
                                        .widget
                                        .toggle_selected();
                                }
                            }
                        }
                    }
                    WidgetCommand::Save { text } => {
                        self.save_to_request_file_and_update_widgets(&text)?;
                    }
                    WidgetCommand::MoveRequestSelection { new_idx } => {
                        self.state.selected_request_idx = Some(new_idx);

                        self.state.request_list_state.select(Some(new_idx));
                        for (_, pane) in &mut self.panes {
                            for widget in &mut pane.widgets {
                                widget.widget.update_shared_state(&self.state)?;
                            }
                        }
                    }
                    WidgetCommand::Quit => {
                        return Ok(());
                    }
                    WidgetCommand::Clear { .. } => {
                        term.clear()?;
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

    let editor_start_state = match &buffer {
        Some(curlman_file_str) => {
            let lines = curlman_file_str
                .split('\n')
                .map(|s| s.to_string())
                .collect::<Vec<_>>();

            Some(lines)
        }
        None => None,
    };

    let editor_widget = Box::new(editor::Editor::new(
        editor_start_state.unwrap_or(vec!["".to_string()]),
    ));

    let output_widget = Box::new(executor::RequestExecutor::new());
    let mut initally_selected_request = None;
    let mut initial_requests_vec = None;

    let request_browser_widget = Box::new(if let Some(buffer) = buffer {
        match parse_curlman_request_file(&buffer) {
            Ok((_, parsed_requests)) => {
                initial_requests_vec = Some(parsed_requests.clone());
                let browser = editor::RequestBrowser::from(parsed_requests);
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
        .constraints([Constraint::Fill(1), Constraint::Percentage(60)]);

    let inner_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(40), Constraint::Percentage(60)]);

    let browser_widgets = vec![PaneWidget::new(
        request_browser_widget,
        1,
        DirectionArray::NONE,
    )];

    let editor_widgets = vec![
        PaneWidget::new(
            editor_widget,
            0,
            DirectionArray([None, None, None, Some(TargetId(1))]),
        ),
        PaneWidget::new(
            output_widget,
            1,
            DirectionArray([None, None, Some(TargetId(0)), None]),
        ),
    ];

    let layouts = vec![parent_layout, inner_layout];

    let mut panes = vec![
        Pane::new(
            browser_widgets,
            Some(LayoutParent::new(0, 0)),
            0,
            DirectionArray([None, Some(TargetId(1)), None, None]),
        ),
        Pane::new(
            editor_widgets,
            Some(LayoutParent::new(0, 1)),
            1,
            DirectionArray([Some(TargetId(0)), None, None, None]),
        ),
    ];

    let initial_state = AppState {
        request_list_state: ListState::default().with_selected(initally_selected_request),
        requests: initial_requests_vec.unwrap_or(Vec::new()),
        selected_request_idx: initally_selected_request,
    };

    for pane in &mut panes {
        for widget in &mut pane.widgets {
            widget.widget.update_shared_state(&initial_state)?;
        }
    }

    let jq_state = unsafe { jq_init() };

    if jq_state.is_null() {
        ratatui::restore();
        disable_raw_mode()?;
        eprintln!("Unable to start jq");
        std::process::exit(1);
    }

    let mut app = App::new(panes, layouts, 1, 0, curlman_file, initial_state, jq_state);

    app.run(&mut terminal)?;

    disable_raw_mode()?;
    Ok(())
}
