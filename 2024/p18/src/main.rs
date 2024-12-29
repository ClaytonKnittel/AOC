use std::{
  collections::{BTreeSet, HashSet},
  fs::File,
  io::{BufReader, Read},
};

use util::{
  error::{AocError, AocResult},
  grid::{Grid, Pos},
};

fn do_dijkstra(maze: &Grid, start: Pos, end: Pos) -> u64 {
  let mut scored_tiles = HashSet::<Pos>::new();
  let mut to_explore = BTreeSet::new();
  to_explore.insert((0, start));

  while let Some((distance, pos)) = to_explore.pop_first() {
    if pos == end {
      return distance;
    }

    if !scored_tiles.insert(pos) {
      continue;
    }

    to_explore.extend(
      maze
        .orthogonal_neighbors(pos)
        .filter(|pos| maze[pos] == b'.')
        .map(|neighbor_pos: Pos| (distance + 1, neighbor_pos)),
    );
  }

  u64::MAX
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  const WIDTH: usize = 71;
  const HEIGHT: usize = 71;
  const FALLEN_BYTES: usize = 1024;

  const START: Pos = Pos { col: 0, row: 0 };
  const END: Pos = Pos {
    col: WIDTH as isize - 1,
    row: HEIGHT as isize - 1,
  };

  let mut input_str = String::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_string(&mut input_str)?;
  let byte_coords: Vec<_> = input_str
    .lines()
    .map(|line| {
      let (col, row) = line
        .split_once(',')
        .ok_or_else(|| AocError::Parse(format!("Line {line} does not contain a ','")))?;
      Ok((col.parse()?, row.parse()?))
    })
    .collect::<AocResult<_>>()?;

  let grid = byte_coords.iter().cloned().take(FALLEN_BYTES).fold(
    Grid::new(vec![vec![b'.'; WIDTH]; HEIGHT]),
    |mut grid, (col, row)| {
      grid[Pos { col, row }] = b'#';
      grid
    },
  );

  let dist_to_end = do_dijkstra(&grid, START, END);
  println!("Distance to end: {dist_to_end}");

  let coord_of_blocker = byte_coords
    .iter()
    .cloned()
    .skip(FALLEN_BYTES)
    .scan(grid, |grid, (col, row)| {
      let pos = Pos { col, row };
      grid[pos] = b'#';
      Some((do_dijkstra(grid, START, END) == u64::MAX).then_some(pos))
    })
    .flatten()
    .next()
    .unwrap();
  println!(
    "Coordinate of blocker: {},{}",
    coord_of_blocker.col, coord_of_blocker.row
  );

  Ok(())
}
