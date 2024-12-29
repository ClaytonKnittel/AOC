use std::collections::{hash_map::Entry, BTreeSet, HashMap, HashSet};

use util::{
  error::{AocError, AocResult},
  grid::{Diff, Grid, Pos},
  parse::parse_grid,
};

fn do_dijkstra(maze: &Grid, start: Pos) -> HashMap<Pos, u64> {
  let mut scored_tiles = HashMap::new();
  let mut to_explore = BTreeSet::new();
  to_explore.insert((0, start));

  while let Some((distance, pos)) = to_explore.pop_first() {
    if let Entry::Vacant(entry) = scored_tiles.entry(pos) {
      entry.insert(distance);
    } else {
      continue;
    }

    to_explore.extend(
      maze
        .orthogonal_neighbors(pos)
        .filter(|pos| maze[pos] == b'.')
        .map(|neighbor_pos: Pos| (distance + 1, neighbor_pos)),
    );
  }

  scored_tiles
}

fn radial_neighbors(maze: &Grid, pos: Pos, radius: u64) -> impl Iterator<Item = (Pos, u64)> + '_ {
  (1..=radius as isize)
    .flat_map(move |r| {
      (0..r).flat_map(move |d| {
        [
          (pos + Diff { dc: r - d, dr: d }, r as u64),
          (pos + Diff { dc: -d, dr: r - d }, r as u64),
          (pos + Diff { dc: -r + d, dr: -d }, r as u64),
          (pos + Diff { dc: d, dr: -r + d }, r as u64),
        ]
        .into_iter()
      })
    })
    .filter(|&(pos, _)| maze.in_bounds(pos))
}

fn compute_cheat_savings(
  maze: &Grid,
  cheat_time: u64,
  pos: Pos,
  pos_distance_from_start: u64,
  fair_distance: u64,
  distances_to_end: &HashMap<Pos, u64>,
) -> HashMap<u64, u64> {
  let mut cheat_savings = HashMap::new();

  for (neighbor, distance) in radial_neighbors(maze, pos, cheat_time) {
    if let Some(distance_to_end) = distances_to_end.get(&neighbor) {
      let total_distance = pos_distance_from_start + distance + distance_to_end;
      if total_distance < fair_distance {
        match cheat_savings.entry(fair_distance - total_distance) {
          Entry::Occupied(mut entry) => {
            *entry.get_mut() += 1;
          }
          Entry::Vacant(entry) => {
            entry.insert(1);
          }
        }
      }
    }
  }

  cheat_savings
}

fn do_cheat_dijkstra(
  maze: &Grid,
  start: Pos,
  end: Pos,
  cheat_time: u64,
  distances_to_end: &HashMap<Pos, u64>,
) -> HashMap<u64, u64> {
  let mut time_saves = HashMap::new();
  let mut explored_tiles = HashSet::new();
  let mut to_explore = BTreeSet::new();
  to_explore.insert((0, start));

  let &fair_distance = distances_to_end.get(&start).unwrap();

  while let Some((distance, pos)) = to_explore.pop_first() {
    if pos == end {
      break;
    }
    if !explored_tiles.insert(pos) {
      continue;
    }

    for (time_save, count) in compute_cheat_savings(
      maze,
      cheat_time,
      pos,
      distance,
      fair_distance,
      distances_to_end,
    ) {
      match time_saves.entry(time_save) {
        Entry::Occupied(mut entry) => {
          *entry.get_mut() += count;
        }
        Entry::Vacant(entry) => {
          entry.insert(count);
        }
      }
    }

    to_explore.extend(
      maze
        .orthogonal_neighbors(pos)
        .filter(|pos| maze[pos] == b'.')
        .map(|neighbor_pos: Pos| (distance + 1, neighbor_pos)),
    );
  }

  time_saves
}

fn total_savings_over(maze: &Grid, start: Pos, end: Pos, cheat_time: u64, min_savings: u64) -> u64 {
  let distances_to_end = do_dijkstra(maze, end);
  let time_saves = do_cheat_dijkstra(maze, start, end, cheat_time, &distances_to_end);
  time_saves
    .into_iter()
    .filter_map(|(savings, count)| (savings >= min_savings).then_some(count))
    .sum()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  const MIN_SAVINGS: u64 = 100;
  let mut grid = parse_grid(INPUT_FILE)?;

  let start = grid
    .find_and_replace(b'S', b'.')
    .ok_or_else(|| AocError::Parse("No 'S' found in puzzle".to_owned()))?;
  let end = grid
    .find_and_replace(b'E', b'.')
    .ok_or_else(|| AocError::Parse("No 'E' found in puzzle".to_owned()))?;

  let total_100s_savings = total_savings_over(&grid, start, end, 2, MIN_SAVINGS);
  println!("Savings >= {MIN_SAVINGS}ps with 2ps cheat: {total_100s_savings}");

  let total_100s_savings = total_savings_over(&grid, start, end, 20, MIN_SAVINGS);
  println!("Savings >= {MIN_SAVINGS}ps with 20ps cheat: {total_100s_savings}");

  Ok(())
}
