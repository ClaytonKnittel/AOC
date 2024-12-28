use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};

use util::{
  error::AocResult,
  grid::{Grid, Pos},
  parse::parse_grid,
};

fn trailhead_score(grid: &Grid, starting_pos: Pos) -> u64 {
  let mut reached_tiles = HashSet::<Pos>::new();
  let mut score = 0;
  let mut to_explore = VecDeque::new();
  to_explore.push_back(starting_pos);

  while let Some(pos) = to_explore.pop_front() {
    let height = grid[pos] - b'0';
    if height == 9 {
      score += 1;
    } else {
      to_explore.extend(
        grid
          .orthogonal_neighbors(pos)
          .filter(|&pos| grid[pos] == height + 1 + b'0' && reached_tiles.insert(pos)),
      );
    }
  }

  score
}

fn trailhead_rating(grid: &Grid, starting_pos: Pos) -> u64 {
  let mut path_count = HashMap::<Pos, u64>::new();
  let mut rating = 0;
  let mut to_explore = VecDeque::new();
  to_explore.push_back(starting_pos);
  path_count.insert(starting_pos, 1);

  while let Some(pos) = to_explore.pop_front() {
    let height = grid[pos] - b'0';
    let ways_to_approach = *path_count.get(&pos).unwrap();

    if height == 9 {
      rating += ways_to_approach;
    } else {
      to_explore.extend(grid.orthogonal_neighbors(pos).filter(|&pos| {
        grid[pos] == height + 1 + b'0'
          && match path_count.entry(pos) {
            Entry::Occupied(mut entry) => {
              *entry.get_mut() += ways_to_approach;
              false
            }
            Entry::Vacant(entry) => {
              entry.insert(ways_to_approach);
              true
            }
          }
      }));
    }
  }

  rating
}

fn sum_of_trailhead_scores(grid: &Grid) -> u64 {
  let mut total_score = 0;
  for row in 0..grid.height() {
    for col in 0..grid.width() {
      let pos = Pos {
        row: row as isize,
        col: col as isize,
      };
      if grid[pos] == b'0' {
        total_score += trailhead_score(grid, pos);
      }
    }
  }

  total_score
}

fn sum_of_trailhead_ratings(grid: &Grid) -> u64 {
  let mut total_rating = 0;
  for row in 0..grid.height() {
    for col in 0..grid.width() {
      let pos = Pos {
        row: row as isize,
        col: col as isize,
      };
      if grid[pos] == b'0' {
        total_rating += trailhead_rating(grid, pos);
      }
    }
  }

  total_rating
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let grid = parse_grid(INPUT_FILE)?;

  let sum_of_scores = sum_of_trailhead_scores(&grid);
  println!("Sum of trailhead scores: {sum_of_scores}");

  let sum_of_ratings = sum_of_trailhead_ratings(&grid);
  println!("Sum of trailhead ratings: {sum_of_ratings}");

  Ok(())
}
