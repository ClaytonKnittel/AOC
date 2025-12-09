use std::str::FromStr;

use itertools::Itertools;
use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
  union_find::UnionFind,
};

use crate::solution::{NumericSolution, Part};

#[derive(Clone)]
struct Coord {
  x: u32,
  y: u32,
  z: u32,
}

impl Coord {
  fn dist2(&self, other: &Self) -> u64 {
    let dx = self.x.abs_diff(other.x) as u64;
    let dy = self.y.abs_diff(other.y) as u64;
    let dz = self.z.abs_diff(other.z) as u64;
    dx * dx + dy * dy + dz * dz
  }
}

impl FromStr for Coord {
  type Err = Box<dyn std::error::Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let mut coords = s.split(',');
    Ok(Self {
      x: coords
        .next()
        .ok_or_else(|| AocError::Parse(format!("Expected \"x,y,z\", got \"{s}\"")))?
        .parse()?,
      y: coords
        .next()
        .ok_or_else(|| AocError::Parse(format!("Expected \"x,y,z\", got \"{s}\"")))?
        .parse()?,
      z: coords
        .next()
        .ok_or_else(|| AocError::Parse(format!("Expected \"x,y,z\", got \"{s}\"")))?
        .parse()?,
    })
  }
}

pub struct P8;

impl NumericSolution for P8 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let boxes: Vec<Coord> = list_of_strings(input_path)?
      .map(|input| input?.parse())
      .collect::<Result<_, _>>()?;
    let n_boxes = boxes.len() as u64;
    let mut uf = UnionFind::from_initializers((0..boxes.len()).map(|idx| (idx, 1)));

    let connections = boxes
      .iter()
      .cloned()
      .enumerate()
      .tuple_combinations()
      .map(|((idx1, c1), (idx2, c2))| (c1.dist2(&c2), idx1, idx2))
      .sorted_by_key(|(dist, _, _)| *dist);

    match part {
      Part::P1 => {
        for (_, idx1, idx2) in connections.take(1000) {
          uf.union(idx1, idx2);
        }

        Ok(
          uf.root_level_keys()
            .into_iter()
            .map(|key| uf.metadata(key))
            .sorted()
            .rev()
            .take(3)
            .product(),
        )
      }
      Part::P2 => {
        for (_, idx1, idx2) in connections {
          let k = uf.union(idx1, idx2);
          if *uf.metadata(k) == n_boxes {
            return Ok(boxes[idx1].x as u64 * boxes[idx2].x as u64);
          }
        }

        Err(
          AocError::Runtime(
            "Unexpected termination before all circuits have reconnected".to_owned(),
          )
          .into(),
        )
      }
    }
  }
}
