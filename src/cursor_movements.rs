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

fn find_word_forward_pos(line: &str) -> Option<usize> {
    let mut in_whitespace = line.chars().next()?.is_whitespace();
    for (idx, ch) in line.char_indices() {
        if in_whitespace && !ch.is_whitespace() {
            return Some(idx);
        }
        in_whitespace = ch.is_whitespace();
    }
    None
}

fn find_word_backward_pos(line: &str) -> Option<usize> {
    let mut in_whitespace = line.chars().last()?.is_whitespace();
    for (idx, ch) in line.char_indices().rev() {
        if !in_whitespace && ch.is_whitespace() {
            return Some(idx + 1); // +1 to point to the start of the word
        }
        in_whitespace = ch.is_whitespace();
    }
    None
}

fn find_word_end_pos(line: &str) -> Option<usize> {
    for (idx, ch) in line.char_indices() {
        if ch.is_ascii_punctuation()
            || line[idx..]
                .chars()
                .nth(1)
                .map_or(true, |next_ch| next_ch.is_whitespace())
        {
            return Some(idx);
        }
    }
    None
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
                let rest_of_line = &lines[row][col..];
                Some((row, find_word_forward_pos(rest_of_line)?))
            }
            WordEnd => {
                let rest_of_line = &lines[row][col..];
                Some((row, find_word_end_pos(rest_of_line)?))
            }
            WordBack => {
                let rest_of_line = &lines[row][..col];
                Some((row, find_word_backward_pos(rest_of_line)?))
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
