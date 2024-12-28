use std::{borrow::Borrow, collections::HashSet, error::Error, str::FromStr};

use once_cell::sync::Lazy;
use regex::Regex;
use util::{
  error::{AocError, AocResult},
  grid::{Diff, Pos},
  math::modulus,
  parse::list_of_objects,
};

struct Robot {
  pos: Pos,
  vel: Diff,
}

impl Robot {
  fn find_position_after(&self, steps: u64, width: usize, height: usize) -> Pos {
    let distance = steps as isize * self.vel;
    let final_position = self.pos + distance;
    Pos {
      col: modulus(final_position.col, width as isize),
      row: modulus(final_position.row, height as isize),
    }
  }
}

impl FromStr for Robot {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    static RE: Lazy<Regex> =
      Lazy::new(|| Regex::new(r"^p=(\d+),(\d+) v=([-0-9]+),([-0-9]+)$").unwrap());

    let captures = RE
      .captures(s)
      .ok_or_else(|| AocError::Parse(format!("Failed to parse as ClawConfig: {s}")))?;

    Ok(Robot {
      pos: Pos {
        col: captures[1].parse()?,
        row: captures[2].parse()?,
      },
      vel: Diff {
        dc: captures[3].parse()?,
        dr: captures[4].parse()?,
      },
    })
  }
}

fn quadrant(pos: Pos, width: usize, height: usize) -> Option<usize> {
  if pos.col == width as isize / 2 || pos.row == height as isize / 2 {
    None
  } else if pos.col < width as isize / 2 {
    if pos.row < height as isize / 2 {
      Some(0)
    } else {
      Some(1)
    }
  } else if pos.row < height as isize / 2 {
    Some(2)
  } else {
    Some(3)
  }
}

trait T: Sized {}

impl T for Robot {}

fn quadrant_product<I, R>(robots: I, steps: u64, width: usize, height: usize) -> u64
where
  I: IntoIterator<Item = R>,
  R: Borrow<Robot>,
{
  robots
    .into_iter()
    .fold(vec![0; 4], |mut quadrants, robot| {
      let final_pos = robot.borrow().find_position_after(steps, width, height);
      if let Some(q_idx) = quadrant(final_pos, width, height) {
        quadrants[q_idx] += 1;
      }
      quadrants
    })
    .into_iter()
    .product()
}

#[allow(dead_code)]
fn bounding_box_area<I, R>(robots: I, steps: u64, width: usize, height: usize) -> isize
where
  I: IntoIterator<Item = R>,
  R: Borrow<Robot>,
{
  let (min, max) = robots.into_iter().fold(
    (
      Pos {
        row: height as isize - 1,
        col: width as isize - 1,
      },
      Pos { row: 0, col: 0 },
    ),
    |(min, max), robot| {
      let final_pos = robot.borrow().find_position_after(steps, width, height);

      (
        Pos {
          row: min.row.min(final_pos.row),
          col: min.col.min(final_pos.col),
        },
        Pos {
          row: max.row.max(final_pos.row),
          col: max.col.max(final_pos.col),
        },
      )
    },
  );

  (max.row - min.row + 1) * (max.col - min.col + 1)
}

fn tightness<I, R>(robots: I, steps: u64, width: usize, height: usize) -> usize
where
  I: IntoIterator<Item = R>,
  R: Borrow<Robot>,
{
  let positions = robots
    .into_iter()
    .map(|robot| robot.borrow().find_position_after(steps, width, height))
    .collect::<HashSet<_>>();

  positions
    .iter()
    .filter(|&&pos| {
      positions.contains(&(pos + Diff { dr: 0, dc: 1 }))
        || positions.contains(&(pos + Diff { dr: 0, dc: -1 }))
        || positions.contains(&(pos + Diff { dr: 1, dc: 0 }))
        || positions.contains(&(pos + Diff { dr: -1, dc: 0 }))
    })
    .count()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  const WIDTH: usize = 101;
  const HEIGHT: usize = 103;

  let robots = list_of_objects(INPUT_FILE)?;

  let product = quadrant_product(robots.iter(), 100, WIDTH, HEIGHT);
  println!("Product of quadrants: {product}");

  let mut max_tightness = 0;
  let mut min_steps = 0;
  for steps in 0..100000 {
    let p = tightness(robots.iter(), steps, WIDTH, HEIGHT);

    if p > max_tightness {
      max_tightness = p;
      min_steps = steps;
    }
  }

  println!("Max tightness: {max_tightness} after {min_steps} steps");

  let mut grid = [[0; WIDTH]; HEIGHT];
  for robot in robots.iter() {
    let pos = robot.find_position_after(min_steps, WIDTH, HEIGHT);
    grid[pos.row as usize][pos.col as usize] += 1;
  }
  println!("Steps: {min_steps}");
  for row in grid {
    for col in row {
      print!("{col}");
    }
    println!();
  }

  Ok(())
}
