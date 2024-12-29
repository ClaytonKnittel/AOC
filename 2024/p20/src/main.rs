use std::collections::{hash_map::Entry, BTreeSet, HashMap, HashSet};

use util::{
  error::{AocError, AocResult},
  grid::{Grid, Pos},
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

fn do_cheat_dijkstra(
  maze: &Grid,
  start: Pos,
  end: Pos,
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

    let mut cheat_savings = HashMap::new();

    for neighbor in maze
      .orthogonal_neighbors(pos)
      .filter(|pos| maze[pos] == b'#')
    {
      cheat_savings.extend(maze.orthogonal_neighbors(neighbor).filter_map(|pos| {
        distances_to_end.get(&pos).and_then(|distance_to_end| {
          let total_distance = distance + 2 + distance_to_end;
          (total_distance < fair_distance).then(|| (pos, fair_distance - total_distance))
        })
      }));
    }

    for (_, time_save) in cheat_savings {
      match time_saves.entry(time_save) {
        Entry::Occupied(mut entry) => {
          *entry.get_mut() += 1;
        }
        Entry::Vacant(entry) => {
          entry.insert(1);
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

fn total_savings_over(maze: &Grid, start: Pos, end: Pos, min_savings: u64) -> u64 {
  let distances_to_end = do_dijkstra(maze, end);
  let time_saves = do_cheat_dijkstra(maze, start, end, &distances_to_end);
  time_saves
    .into_iter()
    .filter_map(|(savings, count)| (savings >= min_savings).then_some(count))
    .sum()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut grid = parse_grid(INPUT_FILE)?;

  let start = grid
    .find_and_replace(b'S', b'.')
    .ok_or_else(|| AocError::Parse("No 'S' found in puzzle".to_owned()))?;
  let end = grid
    .find_and_replace(b'E', b'.')
    .ok_or_else(|| AocError::Parse("No 'E' found in puzzle".to_owned()))?;

  let total_100s_savings = total_savings_over(&grid, start, end, 100);
  println!("Savings >= 100s: {total_100s_savings}");

  Ok(())
}
