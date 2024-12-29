use std::{
  borrow::Borrow,
  fmt::{self, Display, Formatter},
  fs::File,
  io::{BufReader, Read},
  iter::successors,
};

use util::{
  error::{AocError, AocResult},
  grid::{Diff, Grid, Pos},
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Move {
  Up,
  Right,
  Down,
  Left,
}

impl Move {
  fn delta(&self) -> Diff {
    match self {
      Move::Up => Diff { dr: -1, dc: 0 },
      Move::Right => Diff { dr: 0, dc: 1 },
      Move::Down => Diff { dr: 1, dc: 0 },
      Move::Left => Diff { dr: 0, dc: -1 },
    }
  }

  fn from_char(c: char) -> AocResult<Self> {
    match c {
      '^' => Ok(Move::Up),
      '>' => Ok(Move::Right),
      'v' => Ok(Move::Down),
      '<' => Ok(Move::Left),
      _ => Err(AocError::Parse(format!("Cannot parse '{c}' as Move")).into()),
    }
  }
}

#[derive(Clone)]
struct Warehouse {
  grid: Grid,
  pos: Pos,
}

impl Warehouse {
  fn from_input(warehouse_str: &str) -> AocResult<Self> {
    let mut grid = Grid::new(
      warehouse_str
        .lines()
        .map(|line| line.chars().map(|c| c as u8).collect())
        .collect(),
    );
    let initial_pos = grid
      .find_and_replace(b'@', b'.')
      .ok_or_else(|| AocError::Runtime("No `@` found in board".to_owned()))?;
    Ok(Warehouse {
      grid,
      pos: initial_pos,
    })
  }

  fn with_split(&self) -> Self {
    let pos = Pos {
      col: 2 * self.pos.col,
      row: self.pos.row,
    };
    Warehouse {
      grid: Grid::new(
        self
          .grid
          .data()
          .iter()
          .map(|row| {
            row
              .iter()
              .flat_map(|&tile| {
                match tile {
                  b'#' => [b'#', b'#'],
                  b'.' => [b'.', b'.'],
                  b'O' => [b'[', b']'],
                  _ => unreachable!(),
                }
                .into_iter()
              })
              .collect()
          })
          .collect(),
      ),
      pos,
    }
  }

  fn sum_of_coordinates(&self) -> u64 {
    self
      .grid
      .iter()
      .map(|(pos, tile)| {
        if tile == b'O' || tile == b'[' {
          (100 * pos.row + pos.col) as u64
        } else {
          0
        }
      })
      .sum()
  }

  fn make_move(&mut self, m: Move) -> AocResult {
    let delta = m.delta();
    let in_front_pos = self.pos + delta;
    let push_to_pos = successors(Some(in_front_pos), |&pos| Some(pos + delta))
      .find(|pos| self.grid[pos] != b'O')
      .unwrap();
    match self.grid[push_to_pos] {
      // We've hit a wall, do nothing.
      b'#' => Ok(()),
      b'.' => {
        // Swap the item directly in front of us with the empty space found.
        let tmp = self.grid[in_front_pos];
        self.grid[in_front_pos] = self.grid[push_to_pos];
        self.grid[push_to_pos] = tmp;

        // Then move into the tile in front of us.
        self.pos = in_front_pos;
        Ok(())
      }
      c => Err(
        AocError::Runtime(format!(
          "Found non-wall/empty character {} when searching for wall/empty",
          c as char
        ))
        .into(),
      ),
    }
  }

  fn with_move<M>(mut self, m: M) -> AocResult<Self>
  where
    M: Borrow<Move>,
  {
    self.make_move(*m.borrow())?;
    Ok(self)
  }

  fn can_do_split_move(&self, pos: Pos, m: Move) -> bool {
    let delta = m.delta();
    match (self.grid[pos], m) {
      (b'[', Move::Right) | (b']', Move::Left) => self.can_do_split_move(pos + 2 * delta, m),
      (b'[', Move::Down | Move::Up) => {
        self.can_do_split_move(pos + delta, m)
          && self.can_do_split_move(pos + delta + Diff { dc: 1, dr: 0 }, m)
      }
      (b']', Move::Down | Move::Up) => {
        self.can_do_split_move(pos + delta, m)
          && self.can_do_split_move(pos + delta + Diff { dc: -1, dr: 0 }, m)
      }
      (b'.', _) => true,
      (b'#', _) => false,
      _ => unreachable!(),
    }
  }

  fn do_make_split_move(&mut self, pos: Pos, m: Move) {
    let delta = m.delta();
    match (self.grid[pos], m) {
      (b'[', Move::Right) | (b']', Move::Left) => {
        self.do_make_split_move(pos + 2 * delta, m);
        self.grid[pos] = b'.';
        self.grid[pos + delta] = if m == Move::Left { b']' } else { b'[' };
        self.grid[pos + 2 * delta] = if m == Move::Left { b'[' } else { b']' };
      }
      (b'[', Move::Down | Move::Up) => {
        self.do_make_split_move(pos + delta, m);
        self.do_make_split_move(pos + delta + Diff { dc: 1, dr: 0 }, m);
        self.grid[pos + delta] = b'[';
        self.grid[pos + delta + Diff { dc: 1, dr: 0 }] = b']';
        self.grid[pos] = b'.';
        self.grid[pos + Diff { dc: 1, dr: 0 }] = b'.';
      }
      (b']', Move::Down | Move::Up) => {
        self.do_make_split_move(pos + delta, m);
        self.do_make_split_move(pos + delta + Diff { dc: -1, dr: 0 }, m);
        self.grid[pos + delta] = b']';
        self.grid[pos + delta + Diff { dc: -1, dr: 0 }] = b'[';
        self.grid[pos] = b'.';
        self.grid[pos + Diff { dc: -1, dr: 0 }] = b'.';
      }
      (b'.', _) => {}
      _ => unreachable!(),
    }
  }

  fn make_split_move(&mut self, m: Move) {
    let delta = m.delta();
    let in_front_pos = self.pos + delta;
    if self.can_do_split_move(in_front_pos, m) {
      self.do_make_split_move(in_front_pos, m);
      self.pos += delta;
    }
  }

  fn with_split_move<M>(mut self, m: M) -> Self
  where
    M: Borrow<Move>,
  {
    self.make_split_move(*m.borrow());
    self
  }
}

impl Display for Warehouse {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let mut grid = self.grid.clone();
    grid[self.pos] = b'@';
    write!(f, "{grid}")
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut contents = String::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_string(&mut contents)?;
  let (warehouse_str, steps_str) = contents
    .split_once("\n\n")
    .ok_or_else(|| AocError::Parse("No blank line found in file".to_owned()))?;

  let warehouse = Warehouse::from_input(warehouse_str)?;

  let moves = steps_str
    .lines()
    .collect::<String>()
    .chars()
    .map(Move::from_char)
    .collect::<AocResult<Vec<_>>>()?;

  let p1_warehouse = moves
    .iter()
    .try_fold(warehouse.clone(), Warehouse::with_move)?;
  println!("Sum of coordinates: {}", p1_warehouse.sum_of_coordinates());

  let p2_warehouse = moves
    .iter()
    .fold(warehouse.with_split(), Warehouse::with_split_move);
  println!(
    "Sum of split coordinates: {}",
    p2_warehouse.sum_of_coordinates()
  );

  Ok(())
}
