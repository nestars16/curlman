use std::{
    collections::HashMap,
    sync::atomic::{AtomicU32, Ordering},
};
mod custom_widgets;
use crossterm::terminal::enable_raw_mode;
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers, *},
    terminal::disable_raw_mode,
};

use ratatui::{layout::Layout, prelude::*, widgets::WidgetRef, DefaultTerminal};
use tui_textarea::TextArea;

struct PaneParent {
    layout_idx: u32,
    layout_pos_idx: usize,
}

const UP: u8 = 1;
const DOWN: u8 = 2;
const RIGHT: u8 = 3;
const LEFT: u8 = 4;

struct TargetId(usize);
struct AvailableDirections([TargetId; 4]);

impl AvailableDirections {
    const NONE: Self = AvailableDirections([TargetId(0), TargetId(0), TargetId(0), TargetId(0)]);
}

struct PaneWidget {
    widget: Box<dyn WidgetRef>,
    layout_idx: usize,
    available_directions: AvailableDirections,
}

impl PaneWidget {
    fn new(
        widget: Box<dyn WidgetRef>,
        layout_id: usize,
        available_directions: AvailableDirections,
    ) -> Self {
        PaneWidget {
            widget,
            layout_idx: layout_id,
            available_directions,
        }
    }
}

// A pane has a layout and an id, meaning that ever
struct Pane {
    parent: Option<PaneParent>,
    layout_id: u32,
    widgets: Vec<PaneWidget>,
}

impl Pane {
    fn new(widgets: Vec<PaneWidget>, parent: Option<PaneParent>, layout_id: u32) -> Self {
        Pane {
            widgets,
            parent,
            layout_id,
        }
    }
}

struct App<'app> {
    panes: HashMap<u32, Pane>,
    layouts: HashMap<u32, Layout>,
    input: InputHandler<'app>,
}

struct InputHandler<'app> {
    textarea: Option<TextArea<'app>>,
}

impl<'app> InputHandler<'app> {
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
            _ => {}
        }
    }

    fn handle_char_key_presses(&mut self, char: char) {
        match char {
            'h' => {}
            'j' => {}
            'k' => {}
            'l' => {}
            _ => {}
        }
    }

    fn handle_special_key_presses() {}
}

impl<'app> App<'app> {
    fn new(panes: Vec<Pane>, layouts: Vec<Layout>) -> Self {
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
            input: InputHandler {
                textarea: Some(TextArea::default()),
            },
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
                        available_directions,
                    } in pane.widgets.iter()
                    {
                        widget.render_ref(widget_layout[*layout_id], frame.buffer_mut())
                    }
                }
            })?;

            if let event::Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('q') {
                    return Ok(());
                }
            }
        }
    }
}

fn main() -> std::io::Result<()> {
    let mut terminal = ratatui::init();
    terminal.clear()?;
    enable_raw_mode()?;

    let parent_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(20), Constraint::Fill(1)]);

    let mut textarea = TextArea::default();

    let widgets = vec![
        PaneWidget::new(
            Box::new(custom_widgets::get_round_bordered_box()),
            0,
            AvailableDirections::NONE,
        ),
        PaneWidget::new(Box::new(&textarea), 0, AvailableDirections::NONE),
    ];

    let layouts = vec![parent_layout];

    let panes = vec![
        Pane::new(widgets, None, 0),
        Pane::new(
            inner_widgets,
            Some(PaneParent {
                layout_idx: 0,
                layout_pos_idx: 1,
            }),
            1,
        ),
    ];

    let mut app = App::new(panes, layouts);

    app.run(&mut terminal)?;
    ratatui::restore();
    disable_raw_mode()?;

    Ok(())
}
