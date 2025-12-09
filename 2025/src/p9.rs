use std::str::FromStr;

use itertools::Itertools;
use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

#[derive(Clone)]
struct Coord {
  x: u32,
  y: u32,
}

impl Coord {
  fn bb_area(&self, other: &Self) -> u64 {
    let dx = self.x.abs_diff(other.x) as u64 + 1;
    let dy = self.y.abs_diff(other.y) as u64 + 1;
    dx * dy
  }
}

impl FromStr for Coord {
  type Err = Box<dyn std::error::Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let (x, y) = s
      .split_once(',')
      .ok_or_else(|| AocError::Parse(format!("Expected \"x,y\", found \"{s}\"")))?;

    Ok(Self {
      x: x.parse()?,
      y: y.parse()?,
    })
  }
}

pub struct P9;

impl NumericSolution for P9 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let red_tiles: Vec<Coord> = list_of_strings(input_path)?
      .map(|line| line?.parse())
      .collect::<Result<_, _>>()?;

    red_tiles
      .into_iter()
      .tuple_combinations()
      .map(|(tile1, tile2)| tile1.bb_area(&tile2))
      .max()
      .ok_or_else(|| AocError::Parse("Unexpected empty input".to_owned()).into())
  }
}
