use util::{error::AocResult, parse::parse_grid};

use crate::solution::{NumericSolution, Part};

pub struct P4;

impl NumericSolution for P4 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    parse_grid(input_path).map(|grid| {
      grid
        .iter()
        .filter(|&(pos, tile)| {
          tile == b'@' && grid.neighbors(pos).filter(|pos| grid[pos] == b'@').count() < 4
        })
        .count() as u64
    })
  }
}
