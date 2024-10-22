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
use editor::{CurlmanWidget, WidgetCommand};
use gapbuf::GapBuffer;
use ratatui::{layout::Layout, prelude::*, DefaultTerminal};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use types::{DirectionArray, Pane, PaneParent, PaneWidget, TargetId};

struct App {
    panes: HashMap<u32, Pane>,
    layouts: HashMap<u32, Layout>,
    selected_pane_id: u32,
    selected_widget_idx: usize,
}

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
        }
    }

    fn run(&mut self, term: &mut DefaultTerminal) -> std::io::Result<()> {
        loop {
            term.draw(|frame| {
                for (_, pane) in self.panes.iter() {
                    let widget_layout = if let Some(PaneParent {
                        layout_idx,
                        layout_pos_idx,
                    }) = pane.parent
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
                        widget.render_ref(widget_layout[*layout_id], frame.buffer_mut())
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

            match selected_pane_widget_inner.handle_event(event) {
                Some(cmd) => match cmd {
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
                },
                None => {}
            };
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;
    enable_raw_mode()?;

    let parent_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)]);

    let buffer = Arc::new(RwLock::new(GapBuffer::new()));
    let output = curl::RequestExecutor::new(Arc::clone(&buffer));
    let editor_widget = Box::new(editor::Editor::new(buffer));
    let output_widget = Box::new(output);

    let widgets = vec![
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

    let layouts = vec![parent_layout];
    let panes = vec![Pane::new(widgets, None, 0)];
    let mut app = App::new(panes, layouts, 0, 0);

    app.run(&mut terminal)?;

    ratatui::restore();
    disable_raw_mode()?;
    Ok(())
}
