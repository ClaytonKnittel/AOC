use std::{error::Error, str::FromStr};

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

fn quadrant_product<I>(robots: I, steps: u64, width: usize, height: usize) -> u64
where
  I: IntoIterator<Item = Robot>,
{
  robots
    .into_iter()
    .fold(vec![0; 4], |mut quadrants, robot| {
      let final_pos = robot.find_position_after(steps, width, height);
      if let Some(q_idx) = quadrant(final_pos, width, height) {
        quadrants[q_idx] += 1;
      }
      quadrants
    })
    .into_iter()
    .product()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  const WIDTH: usize = 101;
  const HEIGHT: usize = 103;

  let robots = list_of_objects(INPUT_FILE)?;

  let product = quadrant_product(robots, 100, WIDTH, HEIGHT);
  println!("Product of quadrants: {product}");

  Ok(())
}
