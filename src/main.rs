mod curl;
mod editor;
mod error;
mod parser;
mod types;

use crossterm::terminal::enable_raw_mode;
use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::disable_raw_mode,
};
use ratatui::{layout::Layout, prelude::*, DefaultTerminal};
use std::collections::HashMap;
use types::{AvailableDirections, Pane, PaneParent, PaneWidget};

struct App {
    panes: HashMap<u32, Pane>,
    layouts: HashMap<u32, Layout>,
    selected_pane_id: u32,
    selected_widget_idx: usize,
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
                        let parent_layout = self.layouts.get(&layout_idx).unwrap();

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

            if let event::Event::Key(key) = event {
                if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('q') {
                    return Ok(());
                }
            }

            self.panes.get_mut(&self.selected_pane_id).unwrap().widgets[self.selected_widget_idx]
                .widget
                .handle_event(event)
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
    let editor_widget = Box::new(editor::Editor::default());
    let output_widget = Box::new(curl::RequestExecutor::default());

    let widgets = vec![
        PaneWidget::new(editor_widget, 0, AvailableDirections::NONE),
        PaneWidget::new(output_widget, 1, AvailableDirections::NONE),
    ];

    let layouts = vec![parent_layout];
    let panes = vec![Pane::new(widgets, None, 0)];
    let mut app = App::new(panes, layouts, 0, 0);
    app.run(&mut terminal)?;
    ratatui::restore();
    disable_raw_mode()?;
    Ok(())
}
