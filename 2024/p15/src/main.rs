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

#[derive(Clone, Copy)]
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

  fn sum_of_coordinates(&self) -> u64 {
    self
      .grid
      .iter()
      .map(|(pos, tile)| {
        if tile == b'O' {
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

  let warehouse = moves.iter().try_fold(warehouse, Warehouse::with_move)?;
  println!("Sum of coordinates: {}", warehouse.sum_of_coordinates());

  // println!("Warehouse:");
  // println!("{warehouse}");

  Ok(())
}
