use std::{fmt::Display, str::FromStr};

use util::error::{AocError, AocResult};

#[derive(Debug, Clone, Copy)]
pub enum Part {
  P1,
  P2,
}

impl Part {
  pub fn p1(&self) -> bool {
    matches!(self, Self::P1)
  }

  pub fn p2(&self) -> bool {
    matches!(self, Self::P2)
  }
}

impl FromStr for Part {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "1" => Ok(Self::P1),
      "2" => Ok(Self::P2),
      _ => Err(AocError::Parse(format!(
        "Failed to parse \"{s}\" as a part, expected \"1\" or \"2\""
      ))),
    }
  }
}

impl Display for Part {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::P1 => "1",
        Self::P2 => "2",
      }
    )
  }
}

pub trait Solution {
  fn solve(input_path: &str, part: Part) -> AocResult<String>;
}

pub trait NumericSolution {
  fn solve(input_path: &str, part: Part) -> AocResult<u64>;
}

impl<T: NumericSolution> Solution for T {
  fn solve(input_path: &str, part: Part) -> AocResult<String> {
    T::solve(input_path, part).map(|soln| format!("{soln}"))
  }
}
