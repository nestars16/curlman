use ratatui::widgets::{Block, BorderType};

pub fn get_round_bordered_box() -> Block<'static> {
    Block::bordered().border_type(BorderType::Rounded)
}
