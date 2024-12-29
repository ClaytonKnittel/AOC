use std::{
  cmp::Ordering,
  collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
};

use util::{
  direction::Direction,
  error::{AocError, AocResult},
  grid::{Grid, Pos},
  parse::parse_grid,
};

const MOVE_COST: u64 = 1;
const TURN_COST: u64 = 1000;

type Location = (Pos, Direction);

fn do_dijkstra(
  maze: &Grid,
  start: Pos,
  end: Pos,
  scored_tiles: &mut HashMap<Location, (u64, Vec<Option<Location>>)>,
) -> u64 {
  let mut to_explore = BTreeSet::new();
  to_explore.insert((0, start, Direction::Right, None));

  let mut end_score = None;

  while let Some((score, pos, direction, from)) = to_explore.pop_first() {
    if let Some(end_score) = end_score {
      if score > end_score {
        return end_score;
      }
    } else if pos == end {
      end_score = Some(score);
    }

    match scored_tiles.entry((pos, direction)) {
      Entry::Occupied(mut entry) => {
        match score.cmp(&entry.get().0) {
          Ordering::Less => {
            entry.get_mut().1.clear();
            entry.get_mut().1.push(from);
          }
          Ordering::Equal => {
            entry.get_mut().1.push(from);
          }
          _ => {}
        }
        continue;
      }
      Entry::Vacant(entry) => {
        entry.insert((score, vec![from]));
      }
    }

    let next_tile = pos + direction.delta();
    if maze[next_tile] == b'.' {
      to_explore.insert((
        score + MOVE_COST,
        next_tile,
        direction,
        Some((pos, direction)),
      ));
    }
    to_explore.insert((
      score + TURN_COST,
      pos,
      direction.rotate_left(),
      Some((pos, direction)),
    ));
    to_explore.insert((
      score + TURN_COST,
      pos,
      direction.rotate_right(),
      Some((pos, direction)),
    ));
  }

  end_score.unwrap_or(u64::MAX)
}

fn lowest_score(maze: &Grid, start: Pos, end: Pos) -> u64 {
  let mut scored_tiles = HashMap::new();
  do_dijkstra(maze, start, end, &mut scored_tiles)
}

fn all_paths(maze: &Grid, start: Pos, end: Pos) -> u64 {
  let mut scored_tiles = HashMap::new();
  do_dijkstra(maze, start, end, &mut scored_tiles);

  let mut shortest_map = HashSet::new();
  let mut to_explore: Vec<_> = Direction::all_directions()
    .map(|direction| (end, direction))
    .collect();
  while let Some((pos, direction)) = to_explore.pop() {
    if !shortest_map.insert((pos, direction)) {
      continue;
    }

    to_explore.extend(
      scored_tiles
        .get(&(pos, direction))
        .map(|(_, predecessors)| predecessors.iter().flatten())
        .into_iter()
        .flatten(),
    );
  }

  shortest_map
    .into_iter()
    .map(|(pos, _)| pos)
    .collect::<HashSet<_>>()
    .len() as u64
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

  let all_paths = all_paths(&maze, start, end);
  println!("All paths: {all_paths}");

  Ok(())
}
