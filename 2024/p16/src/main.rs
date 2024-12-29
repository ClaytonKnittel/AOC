use std::collections::{BTreeSet, HashSet};

use util::{
  direction::Direction,
  error::{AocError, AocResult},
  grid::{Grid, Pos},
  parse::parse_grid,
};

const MOVE_COST: u64 = 1;
const TURN_COST: u64 = 1000;

fn lowest_score(maze: &Grid, start: Pos, end: Pos) -> u64 {
  let mut to_explore = BTreeSet::new();
  to_explore.insert((0, start, Direction::Right));
  let mut scored_tiles = HashSet::new();

  while let Some((score, pos, direction)) = to_explore.pop_first() {
    if pos == end {
      return score;
    }

    if !scored_tiles.insert((pos, direction)) {
      continue;
    }

    let next_tile = pos + direction.delta();
    if maze[next_tile] == b'.' {
      to_explore.insert((score + MOVE_COST, next_tile, direction));
    }
    to_explore.insert((score + TURN_COST, pos, direction.rotate_left()));
    to_explore.insert((score + TURN_COST, pos, direction.rotate_right()));
  }

  u64::MAX
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut maze = parse_grid(INPUT_FILE)?;
  let start = maze
    .find_and_replace(b'S', b'.')
    .ok_or_else(|| AocError::Parse("Failed to find 'S' in maze".to_owned()))?;
  let end = maze
    .find_and_replace(b'E', b'.')
    .ok_or_else(|| AocError::Parse("Failed to find 'E' in maze".to_owned()))?;

  let lowest_score = lowest_score(&maze, start, end);
  println!("Lowest score: {lowest_score}");

  Ok(())
}
