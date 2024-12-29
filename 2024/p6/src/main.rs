use util::{direction::Direction, error::AocResult, parse::list_of_chars};

struct DirectionMask {
  mask: u8,
}

impl DirectionMask {
  fn new() -> Self {
    Self { mask: 0 }
  }

  fn empty(&self) -> bool {
    self.mask == 0
  }

  fn mark(&mut self, direction: Direction) -> bool {
    let old_mask = self.mask;
    self.mask = old_mask
      | match direction {
        Direction::Up => 0x1,
        Direction::Right => 0x2,
        Direction::Down => 0x4,
        Direction::Left => 0x8,
      };

    self.mask != old_mask
  }
}

/// Returns Some(visited_tile_count) if the guard exited, or None if the guard was stuck in a loop.
fn count_unique_spaces_helper(
  grid: &[Vec<char>],
  mut visited_tiles: Vec<Vec<DirectionMask>>,
  (width, height): (usize, usize),
  (row, col): (usize, usize),
  direction: Direction,
  mut visited_tile_count: u32,
) -> Option<u32> {
  if visited_tiles[row][col].empty() {
    visited_tile_count += 1;
  }
  if !visited_tiles[row][col].mark(direction) {
    return None;
  }

  let (next_row, next_col) = match direction {
    Direction::Up => {
      if row == 0 {
        return Some(visited_tile_count);
      }
      (row - 1, col)
    }
    Direction::Right => {
      if col == width - 1 {
        return Some(visited_tile_count);
      }
      (row, col + 1)
    }
    Direction::Down => {
      if row == height - 1 {
        return Some(visited_tile_count);
      }
      (row + 1, col)
    }
    Direction::Left => {
      if col == 0 {
        return Some(visited_tile_count);
      }
      (row, col - 1)
    }
  };

  match grid[next_row][next_col] {
    '#' => count_unique_spaces_helper(
      grid,
      visited_tiles,
      (width, height),
      (row, col),
      direction.rotate_right(),
      visited_tile_count,
    ),
    _ => count_unique_spaces_helper(
      grid,
      visited_tiles,
      (width, height),
      (next_row, next_col),
      direction,
      visited_tile_count,
    ),
  }
}

fn count_unique_spaces(grid: &[Vec<char>], pos: (usize, usize)) -> Option<u32> {
  let (width, height) = (grid.len(), grid[0].len());
  count_unique_spaces_helper(
    grid,
    (0..width)
      .map(|_| (0..height).map(|_| DirectionMask::new()).collect())
      .collect(),
    (width, height),
    pos,
    Direction::Up,
    1,
  )
}

fn count_cycles(grid: &[Vec<char>], pos: (usize, usize)) -> u32 {
  let (width, height) = (grid.len(), grid[0].len());

  (0..width)
    .flat_map(|row_idx| {
      (0..height).filter(move |&col_idx| {
        if grid[row_idx][col_idx] != '.' {
          return false;
        }

        let mut grid = grid.to_owned();
        grid[row_idx][col_idx] = '#';
        count_unique_spaces(&grid, pos).is_none()
      })
    })
    .count() as u32
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let grid = list_of_chars(INPUT_FILE)?;

  // Find start tile
  let (row, col) = grid
    .iter()
    .enumerate()
    .flat_map(|(row_idx, row)| {
      row.iter().enumerate().flat_map(move |(col_idx, &tile)| {
        if tile == '^' {
          Some((row_idx, col_idx))
        } else {
          None
        }
      })
    })
    .next()
    .unwrap();
  println!("Start: {row}, {col}");

  println!(
    "Unique spaces: {:?}",
    count_unique_spaces(&grid, (row, col))
  );
  println!("Cycle-causing tiles: {:?}", count_cycles(&grid, (row, col)));

  Ok(())
}
