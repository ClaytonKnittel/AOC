use std::collections::{hash_map::Entry, HashMap, HashSet};

use util::{error::AocResult, parse::list_of_chars};

fn list_antinodes(
  t1: (usize, usize),
  t2: (usize, usize),
  (width, height): (usize, usize),
) -> impl Iterator<Item = (usize, usize)> {
  let r_min = t1.0.min(t2.0);
  let r_max = t1.0.max(t2.0);
  let c_min = t1.1.min(t2.1);
  let c_max = t1.1.max(t2.1);

  let rows = r_max - r_min;
  let cols = c_max - c_min;

  let has_inner = rows % 3 == 0 && cols % 3 == 0;
  if (t1.0 < t2.0) == (t1.1 < t2.1) {
    [
      has_inner.then_some((r_min + rows / 3, c_min + cols / 3)),
      has_inner.then_some((r_min + rows * 2 / 3, c_min + cols * 2 / 3)),
      (r_max + rows < height && c_max + cols < width).then_some((r_max + rows, c_max + cols)),
      (r_min >= rows && c_min >= cols)
        .then_some((r_min.wrapping_sub(rows), c_min.wrapping_sub(cols))),
    ]
  } else {
    [
      has_inner.then_some((r_min + rows / 3, c_min + cols * 2 / 3)),
      has_inner.then_some((r_min + rows * 2 / 3, c_min + cols / 3)),
      (r_max + rows < height && c_min >= cols).then_some((r_max + rows, c_min.wrapping_sub(cols))),
      (r_min >= rows && c_max + cols < width).then_some((r_min.wrapping_sub(rows), c_max + cols)),
    ]
  }
  .into_iter()
  .flatten()
}

fn count_antinodes(grid: &[Vec<char>]) -> u32 {
  let mut antinodes = HashSet::new();
  let mut antennae = HashMap::<char, Vec<(usize, usize)>>::new();
  let wh = (grid[0].len(), grid.len());

  for (row_idx, row) in grid.iter().enumerate() {
    for (col_idx, &label) in row.iter().enumerate() {
      if label == '.' {
        continue;
      }

      let pos = (row_idx, col_idx);
      for &other_pos in antennae.get(&label).unwrap_or(&vec![]) {
        list_antinodes(pos, other_pos, wh).for_each(|antinode_pos| {
          antinodes.insert(antinode_pos);
        });
      }

      match antennae.entry(label) {
        Entry::Occupied(mut entry) => {
          entry.get_mut().push(pos);
        }
        Entry::Vacant(entry) => {
          entry.insert(vec![pos]);
        }
      }
    }
  }

  antinodes.len() as u32
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let grid = list_of_chars(INPUT_FILE)?;

  let num_antinodes = count_antinodes(&grid);
  println!("Num antinodes: {num_antinodes}");

  Ok(())
}
