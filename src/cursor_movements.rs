use std::cmp;

#[derive(Clone, Debug)]
pub enum CursorMoveDirection {
    Forward,
    Back,
    Up,
    Down,
    Head,
    End,
    Top,
    Bottom,
    WordForward,
    WordEnd,
    WordBack,
    Jump(u16, u16),
}

impl CursorMoveDirection {
    pub fn next_cursor(
        &self,
        (row, col): (usize, usize),
        lines: &[String],
    ) -> Option<(usize, usize)> {
        use CursorMoveDirection::*;

        fn get_the_beginning_byte_of_last_character(line: &str) -> usize {
            let mut beginning_byte_of_last_character = line.len();
            while !line.is_char_boundary(beginning_byte_of_last_character) {
                beginning_byte_of_last_character -= 1
            }
            beginning_byte_of_last_character
        }

        fn fit_col(col: usize, line: &str) -> usize {
            if col >= line.len() {
                get_the_beginning_byte_of_last_character(line)
            } else if !line.is_char_boundary(col) {
                let mut beginning_of_utf8_char_near_col = col - 1;
                while !line.is_char_boundary(beginning_of_utf8_char_near_col) {
                    beginning_of_utf8_char_near_col -= 1;
                }
                beginning_of_utf8_char_near_col
            } else {
                col
            }
        }

        match self {
            Forward if col >= lines[row].len() => (row + 1 < lines.len()).then(|| (row + 1, 0)),
            Forward => {
                let mut bytes_forward = 1;

                while !lines[row].is_char_boundary(col + bytes_forward) {
                    bytes_forward += 1
                }

                Some((row, col + bytes_forward))
            }
            Back if col == 0 => {
                let row = row.checked_sub(1)?;
                Some((row, get_the_beginning_byte_of_last_character(&lines[row])))
            }
            Back => {
                let current_line = lines[row].as_str();
                let mut bytes_backward = 1;
                while !current_line.is_char_boundary(col - bytes_backward) {
                    bytes_backward += 1
                }
                Some((row, col.checked_sub(bytes_backward)?))
            }
            Up => {
                let row = row.checked_sub(1)?;
                Some((row, fit_col(col, &lines[row])))
            }
            Down => Some((row + 1, fit_col(col, lines.get(row + 1)?))),
            Head => Some((row, 0)),
            End => Some((row, lines[row].chars().count())),
            Top => Some((0, fit_col(col, &lines[0]))),
            Bottom => {
                let row = lines.len() - 1;
                Some((row, fit_col(col, &lines[row])))
            }
            WordForward => {
                let line = &lines[row];
                let other_half_of_line = &line[..col];
                let rest_of_line = &line[col..];
                let mut has_seen_punctuation = false;
                let mut has_seen_whitespace = false;

                let is_first_punctuation = rest_of_line
                    .chars()
                    .into_iter()
                    .nth(0)
                    .is_none_or(|s| s.is_ascii_punctuation());

                for (idx, ch) in rest_of_line.char_indices() {
                    let is_stopper = ch.is_whitespace() || ch.is_ascii_punctuation();

                    if ch.is_whitespace() {
                        has_seen_whitespace = true;
                        continue;
                    }

                    if ch.is_ascii_punctuation() {
                        has_seen_punctuation = true;
                    }

                    match (has_seen_whitespace, has_seen_punctuation) {
                        (true, true) | (true, false) => {
                            if !is_stopper {
                                return Some((row, idx + other_half_of_line.len()));
                            }
                        }
                        (false, true) => {
                            if !is_first_punctuation {
                                return Some((row, idx + other_half_of_line.len()));
                            } else if !is_stopper {
                                return Some((row, idx + other_half_of_line.len()));
                            }
                        }
                        (false, false) => continue,
                    }
                }

                match lines.get(row + 1) {
                    Some(next_line) => Some((
                        row + 1,
                        next_line
                            .char_indices()
                            .into_iter()
                            .find(|(_, ch)| !ch.is_whitespace())
                            .unwrap_or((0, ' '))
                            .0,
                    )),
                    None => Some((row, line.char_indices().last().unwrap_or((0, ' ')).0)),
                }
            }
            WordEnd => {
                let line = &lines[row][col..];
                let behind_cursor = &lines[row][..col];
                let mut prev: Option<(usize, char)> = None;

                for (idx, ch) in line.char_indices() {
                    if ch.is_whitespace() || ch.is_ascii_punctuation() && ch != '/' {
                        if let Some((p_idx, p_ch)) = prev {
                            if p_ch.is_whitespace() {
                                continue;
                            }

                            if p_idx != 0 {
                                return Some((row, p_idx + behind_cursor.len()));
                            }
                        }
                    }
                    prev = Some((idx, ch));
                }

                match lines.get(row + 1) {
                    Some(next_line) => Some((
                        row + 1,
                        next_line
                            .char_indices()
                            .into_iter()
                            .find(|(_, ch)| !ch.is_whitespace())
                            .unwrap_or((0, ' '))
                            .0,
                    )),
                    None => Some((row, line.char_indices().last().unwrap_or((0, ' ')).0)),
                }
            }
            WordBack => {
                //curl https://google.com
                let line = &lines[row];
                let left_of_cursor = &line[..col];
                let mut prev = None;

                for (idx, ch) in left_of_cursor.char_indices().rev() {
                    fn is_stopper(ch: char) -> bool {
                        ch.is_whitespace() || ch.is_ascii_punctuation()
                    }

                    if is_stopper(ch) {
                        match prev {
                            Some((p_idx, p_ch)) if !is_stopper(p_ch) => {
                                return Some((row, p_idx));
                            }
                            _ => {}
                        }
                    }

                    prev = Some((idx, ch))
                }

                match lines.get(row - 1) {
                    Some(_) => Some((
                        row - 1,
                        lines[row].char_indices().last().unwrap_or((0, ' ')).0,
                    )),
                    None => Some((row, lines[row].char_indices().nth(0).unwrap_or((0, ' ')).0)),
                }
            }
            Jump(row, col) => {
                let row = cmp::min(*row as usize, lines.len() - 1);
                let col = fit_col(*col as usize, &lines[row]);
                Some((row, col))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CursorMovement {
    Regular(CursorMoveDirection),
    Until(char),
    WholeLine,
}

impl TryFrom<CursorMovement> for CursorMoveDirection {
    type Error = ();

    fn try_from(value: CursorMovement) -> Result<Self, Self::Error> {
        match value {
            CursorMovement::Regular(cursor_move) => Ok(cursor_move),
            CursorMovement::Until(_) => Err(()),
            CursorMovement::WholeLine => Err(()),
        }
    }
}
