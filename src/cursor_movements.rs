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

        fn fit_col(col: usize, line: &str) -> usize {
            cmp::min(col, line.chars().count())
        }

        match self {
            Forward if col >= lines[row].chars().count() => {
                (row + 1 < lines.len()).then(|| (row + 1, 0))
            }
            Forward => Some((row, col + 1)),
            Back if col == 0 => {
                let row = row.checked_sub(1)?;
                Some((row, lines[row].chars().count()))
            }
            Back => Some((row, col - 1)),
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
            WordEnd => {
                // `+ 1` for not accepting the current cursor position
                if let Some(col) = find_word_inclusive_end_forward(&lines[row], col + 1) {
                    Some((row, col))
                } else {
                    let mut row = row;
                    loop {
                        if row == lines.len() - 1 {
                            break Some((row, lines[row].chars().count()));
                        }
                        row += 1;
                        if let Some(col) = find_word_inclusive_end_forward(&lines[row], 0) {
                            break Some((row, col));
                        }
                    }
                }
            }
            WordForward => {
                if let Some(col) = find_word_start_forward(&lines[row], col) {
                    Some((row, col))
                } else if row + 1 < lines.len() {
                    Some((row + 1, 0))
                } else {
                    Some((row, lines[row].chars().count()))
                }
            }
            WordBack => {
                if let Some(col) = find_word_start_backward(&lines[row], col) {
                    Some((row, col))
                } else if row > 0 {
                    Some((row - 1, lines[row - 1].chars().count()))
                } else {
                    Some((row, 0))
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
