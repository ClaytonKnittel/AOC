use util::{error::AocResult, grid::Grid, parse::parse_grid};

use crate::solution::{NumericSolution, Part};

const MAX_ADJ_ROLLS: usize = 3;

fn count_removed_p1(grid: Grid) -> u64 {
  grid
    .iter()
    .filter(|&(_, tile)| tile == b'@')
    .filter(|&(pos, _)| {
      grid
        .neighbors(pos)
        .filter(|(_, tile)| *tile == b'@')
        .count()
        <= MAX_ADJ_ROLLS
    })
    .count() as u64
}

fn count_removed_p2(mut grid: Grid) -> u64 {
  let mut tiles_removed = 0;
  for pos in grid.positions() {
    if grid[pos] == b'@'
      && grid
        .neighbors(pos)
        .filter(|(_, tile)| *tile == b'@')
        .count()
        <= MAX_ADJ_ROLLS
    {
      grid[pos] = b'.';
      tiles_removed += 1;
    }
  }

  if tiles_removed > 0 {
    tiles_removed + count_removed_p2(grid)
  } else {
    0
  }
}

pub struct P4;

impl NumericSolution for P4 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    parse_grid(input_path).map(|grid| match part {
      Part::P1 => count_removed_p1(grid),
      Part::P2 => count_removed_p2(grid),
    })
  }
}
