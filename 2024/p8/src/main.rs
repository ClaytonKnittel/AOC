use std::{
  collections::{hash_map::Entry, HashMap, HashSet},
  iter,
};

use util::{error::AocResult, math::gcd, parse::list_of_chars};

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

fn all_multiples(
  start: (isize, isize),
  diff: (isize, isize),
  (width, height): (isize, isize),
) -> impl Iterator<Item = (isize, isize)> {
  iter::successors(Some(start), move |(row, col)| {
    let (dr, dc) = diff;
    ((0..height).contains(&(row + dr)) && (0..width).contains(&(col + dc)))
      .then_some((row + dr, col + dc))
  })
}

fn list_antinodes_p2(
  t1: (usize, usize),
  t2: (usize, usize),
  (width, height): (usize, usize),
) -> impl Iterator<Item = (usize, usize)> {
  let t1 = (t1.0 as isize, t1.1 as isize);
  let t2 = (t2.0 as isize, t2.1 as isize);

  let r_min = t1.0.min(t2.0);
  let r_max = t1.0.max(t2.0);
  let c_min = t1.1.min(t2.1);
  let c_max = t1.1.max(t2.1);

  let rows = r_max - r_min;
  let cols = c_max - c_min;

  let divisor = gcd(rows, cols);
  all_multiples(
    t1,
    ((t2.0 - t1.0) / divisor, (t2.1 - t1.1) / divisor),
    (width as isize, height as isize),
  )
  .chain(all_multiples(
    t1,
    ((t1.0 - t2.0) / divisor, (t1.1 - t2.1) / divisor),
    (width as isize, height as isize),
  ))
  .map(move |(row, col)| (row as usize, col as usize))
}

fn count_antinodes<F, I>(grid: &[Vec<char>], mut antinode_counter: F) -> u32
where
  F: FnMut((usize, usize), (usize, usize), (usize, usize)) -> I,
  I: Iterator<Item = (usize, usize)>,
{
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
        antinode_counter(pos, other_pos, wh).for_each(|antinode_pos| {
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

  let num_antinodes = count_antinodes(&grid, list_antinodes);
  println!("Num antinodes: {num_antinodes}");

  let num_antinodes2 = count_antinodes(&grid, list_antinodes_p2);
  println!("Num antinodes 2: {num_antinodes2}");

  Ok(())
}
