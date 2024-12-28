use std::{borrow::Borrow, fmt::Display, ops::Index};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pos {
  pub row: usize,
  pub col: usize,
}

impl Display for Pos {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}, {})", self.row, self.col)
  }
}

pub struct Grid {
  grid: Vec<Vec<u8>>,
}

impl Grid {
  pub fn new(grid: Vec<Vec<u8>>) -> Self {
    Self { grid }
  }

  pub fn width(&self) -> usize {
    self.grid.first().map(|row| row.len()).unwrap_or(0)
  }

  pub fn height(&self) -> usize {
    self.grid.len()
  }

  pub fn orthogonal_neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> {
    [
      (pos.row != 0).then_some(Pos {
        row: pos.row.wrapping_sub(1),
        ..pos
      }),
      (pos.col != self.width() - 1).then_some(Pos {
        col: pos.col + 1,
        ..pos
      }),
      (pos.row != self.height() - 1).then_some(Pos {
        row: pos.row + 1,
        ..pos
      }),
      (pos.col != 0).then_some(Pos {
        col: pos.col.wrapping_sub(1),
        ..pos
      }),
    ]
    .into_iter()
    .flatten()
  }

  pub fn top_left_orthogonal_neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> {
    [
      (pos.row != 0).then_some(Pos {
        row: pos.row.wrapping_sub(1),
        ..pos
      }),
      (pos.col != 0).then_some(Pos {
        col: pos.col.wrapping_sub(1),
        ..pos
      }),
    ]
    .into_iter()
    .flatten()
  }

  pub fn positions(&self) -> impl Iterator<Item = Pos> + '_ {
    (0..self.height()).flat_map(|row| (0..self.width()).map(move |col| Pos { row, col }))
  }

  pub fn iter(&self) -> impl Iterator<Item = (Pos, u8)> + '_ {
    self.grid.iter().enumerate().flat_map(|(row_idx, row)| {
      row.iter().enumerate().map(move |(col_idx, &tile)| {
        (
          Pos {
            row: row_idx,
            col: col_idx,
          },
          tile,
        )
      })
    })
  }
}

impl<P> Index<P> for Grid
where
  P: Borrow<Pos>,
{
  type Output = u8;

  fn index(&self, index: P) -> &Self::Output {
    &self.grid[index.borrow().row][index.borrow().col]
  }
}
