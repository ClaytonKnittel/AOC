use crate::grid::Diff;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Direction {
  Up,
  Right,
  Down,
  Left,
}

impl Direction {
  pub fn rotate_right(self) -> Self {
    match self {
      Self::Up => Self::Right,
      Self::Right => Self::Down,
      Self::Down => Self::Left,
      Self::Left => Self::Up,
    }
  }

  pub fn rotate_left(self) -> Self {
    match self {
      Self::Up => Self::Left,
      Self::Right => Self::Up,
      Self::Down => Self::Right,
      Self::Left => Self::Down,
    }
  }

  pub fn delta(&self) -> Diff {
    match self {
      Self::Up => Diff { dr: -1, dc: 0 },
      Self::Right => Diff { dr: 0, dc: 1 },
      Self::Down => Diff { dr: 1, dc: 0 },
      Self::Left => Diff { dr: 0, dc: -1 },
    }
  }

  pub fn all_directions() -> impl Iterator<Item = Self> {
    [Self::Up, Self::Right, Self::Down, Self::Left].into_iter()
  }
}
