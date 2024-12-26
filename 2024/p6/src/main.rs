use util::{error::AocResult, parse::list_of_chars};

enum Direction {
  Up,
  Right,
  Down,
  Left,
}

impl Direction {
  fn rotate_right(self) -> Self {
    match self {
      Direction::Up => Direction::Right,
      Direction::Right => Direction::Down,
      Direction::Down => Direction::Left,
      Direction::Left => Direction::Up,
    }
  }
}

fn count_unique_spaces_helper(
  mut grid: Vec<Vec<char>>,
  (width, height): (usize, usize),
  (row, col): (usize, usize),
  direction: Direction,
  mut visited_tile_count: u32,
) -> u32 {
  if grid[row][col] == '.' {
    grid[row][col] = 'X';
    visited_tile_count += 1;
  }

  let (next_row, next_col) = match direction {
    Direction::Up => {
      if row == 0 {
        return visited_tile_count;
      }
      (row - 1, col)
    }
    Direction::Right => {
      if col == width - 1 {
        return visited_tile_count;
      }
      (row, col + 1)
    }
    Direction::Down => {
      if row == height - 1 {
        return visited_tile_count;
      }
      (row + 1, col)
    }
    Direction::Left => {
      if col == 0 {
        return visited_tile_count;
      }
      (row, col - 1)
    }
  };

  match grid[next_row][next_col] {
    '#' => count_unique_spaces_helper(
      grid,
      (width, height),
      (row, col),
      direction.rotate_right(),
      visited_tile_count,
    ),
    _ => count_unique_spaces_helper(
      grid,
      (width, height),
      (next_row, next_col),
      direction,
      visited_tile_count,
    ),
  }
}

fn count_unique_spaces(grid: Vec<Vec<char>>, pos: (usize, usize)) -> u32 {
  let wh = (grid.len(), grid[0].len());
  count_unique_spaces_helper(grid, wh, pos, Direction::Up, 1)
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

  println!("Unique spaces: {}", count_unique_spaces(grid, (row, col)));

  Ok(())
}
