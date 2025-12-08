use std::str::FromStr;

use itertools::Itertools;
use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

enum Op {
  Add,
  Mul,
}

impl FromStr for Op {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "+" => Ok(Self::Add),
      "*" => Ok(Self::Mul),
      _ => Err(AocError::Parse(format!(
        "Failed to parse \"{s}\" as an op."
      ))),
    }
  }
}

pub struct P6;

impl NumericSolution for P6 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let lines: Vec<_> = list_of_strings(input_path)?
      .map_ok(|row| {
        row
          .split_ascii_whitespace()
          .map(|input| input.to_owned())
          .collect_vec()
      })
      .collect::<Result<_, _>>()?;

    let inputs: Vec<Vec<u64>> = lines[..lines.len() - 1].iter().try_fold(
      vec![Vec::new(); lines[0].len()],
      |mut v, line| -> AocResult<_> {
        for (vec, el) in v.iter_mut().zip(line) {
          vec.push(
            el.parse()
              .map_err(|err| AocError::Parse(format!("Failed to parse \"{el}\" as u64: {err}")))?,
          );
        }
        Ok(v)
      },
    )?;

    let operators: Vec<Op> = lines[lines.len() - 1]
      .iter()
      .map(|op| op.parse())
      .collect::<Result<_, _>>()?;

    Ok(
      operators
        .iter()
        .zip(inputs)
        .map(|(op, inputs)| match op {
          Op::Add => inputs.iter().sum::<u64>(),
          Op::Mul => inputs.iter().product(),
        })
        .sum(),
    )
  }
}
