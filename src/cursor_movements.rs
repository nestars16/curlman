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

#[derive(PartialEq, Eq, Clone, Copy)]
enum CharKind {
    Space,
    Punct,
    Other,
}

impl CharKind {
    fn new(c: char) -> Self {
        if c.is_whitespace() {
            Self::Space
        } else if c.is_ascii_punctuation() {
            Self::Punct
        } else {
            Self::Other
        }
    }
}

pub fn find_word_start_forward(line: &str, start_col: usize) -> Option<usize> {
    let mut it = line.chars().enumerate().skip(start_col);
    let mut prev = CharKind::new(it.next()?.1);
    for (col, c) in it {
        let cur = CharKind::new(c);
        if cur != CharKind::Space && prev != cur {
            return Some(col);
        }
        prev = cur;
    }
    None
}

pub fn find_word_inclusive_end_forward(line: &str, start_col: usize) -> Option<usize> {
    let mut it = line.chars().enumerate().skip(start_col);
    let (mut last_col, c) = it.next()?;
    let mut prev = CharKind::new(c);
    for (col, c) in it {
        let cur = CharKind::new(c);
        if prev != CharKind::Space && cur != prev {
            return Some(col.saturating_sub(1));
        }
        prev = cur;
        last_col = col;
    }
    if prev != CharKind::Space {
        Some(last_col)
    } else {
        None
    }
}

pub fn find_word_start_backward(line: &str, start_col: usize) -> Option<usize> {
    let idx = line
        .char_indices()
        .nth(start_col)
        .map(|(i, _)| i)
        .unwrap_or(line.len());
    let mut it = line[..idx].chars().rev().enumerate();
    let mut cur = CharKind::new(it.next()?.1);
    for (i, c) in it {
        let next = CharKind::new(c);
        if cur != CharKind::Space && next != cur {
            return Some(start_col - i);
        }
        cur = next;
    }
    (cur != CharKind::Space).then(|| 0)
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
                    bytes_backward -= 1
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
            WordForward => None,
            WordEnd => None,
            WordBack => None,
            Jump(_, _) => None,
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
