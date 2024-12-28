use std::{
  error::Error,
  fs::File,
  io::{BufReader, Read},
  str::FromStr,
};

use once_cell::sync::Lazy;
use regex::Regex;
use util::{
  error::{AocError, AocResult},
  grid::{Diff, Pos},
};

struct ClawConfig {
  a: Diff,
  b: Diff,
  prize: Pos,
}

impl ClawConfig {
  const A_COST: u32 = 3;
  const B_COST: u32 = 1;

  fn min_cost(&self) -> Option<u32> {
    if self.a.dc * self.b.dr == self.a.dr * self.b.dc {
      unreachable!();
    }

    let a_steeper = self.a.dr * self.b.dc > self.b.dr * self.a.dc;
    let (steeper, shallower) = if a_steeper {
      (self.a, self.b)
    } else {
      (self.b, self.a)
    };

    // Binary search for correct ratio of steeper to shallower. Steeper has a
    // greater dr / dc ratio, so dr will always be the "limiting factor".
    let max_steeper = self.prize.row / (steeper.dr as usize);
    let mut low = 0;
    let mut high = max_steeper + 1;
    while low + 1 != high {
      let mid = (low + high) / 2;

      // Compare slopes of (target - mid * steeper) and shallower.
      let steeper_ray = (mid as isize) * steeper;
      if steeper_ray.dr as usize >= self.prize.row || steeper_ray.dc as usize >= self.prize.col {
        high = mid;
        continue;
      }
      let remainder = self.prize - steeper_ray;

      // If remainder is steeper than shallower, then `mid` is too low.
      if remainder.row * (shallower.dc as usize) >= (shallower.dr as usize) * remainder.col {
        low = mid;
      } else {
        high = mid;
      }
    }

    let steeper_ray = (low as isize) * steeper;
    let shallower_count = (self.prize.col as isize - steeper_ray.dc) / shallower.dc;

    (Pos::zero() + steeper_ray + shallower_count * shallower == self.prize).then_some(
      low as u32
        * if a_steeper {
          Self::A_COST
        } else {
          Self::B_COST
        }
        + (shallower_count as u32)
          * if a_steeper {
            Self::B_COST
          } else {
            Self::A_COST
          },
    )
  }
}

impl FromStr for ClawConfig {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    static RE: Lazy<Regex> = Lazy::new(|| {
      Regex::new(
        r"^Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)$",
      )
      .unwrap()
    });

    let captures = RE
      .captures(s)
      .ok_or_else(|| AocError::Parse(format!("Failed to parse as ClawConfig: {s}")))?;

    let a = Diff {
      dc: captures[1].parse()?,
      dr: captures[2].parse()?,
    };
    let b = Diff {
      dc: captures[3].parse()?,
      dr: captures[4].parse()?,
    };
    let prize = Pos {
      col: captures[5].parse()?,
      row: captures[6].parse()?,
    };

    Ok(ClawConfig { a, b, prize })
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut file = String::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_string(&mut file)?;

  let claws = file
    .split("\n\n")
    .map(|claw_str| claw_str.parse::<ClawConfig>())
    .collect::<AocResult<Vec<_>>>()?;

  let min_cost = claws.iter().flat_map(|claw| claw.min_cost()).sum::<u32>();
  println!("Min cost: {min_cost}");

  Ok(())
}
