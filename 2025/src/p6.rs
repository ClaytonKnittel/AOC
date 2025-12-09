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

fn build_grid(input_path: &str) -> AocResult<(Vec<Vec<String>>, Vec<Op>)> {
  let mut strs: Vec<_> = list_of_strings(input_path)?.collect::<Result<_, _>>()?;
  let ops = strs
    .pop()
    .ok_or_else(|| AocError::Parse("Unexpected empty input".to_owned()))?;
  let mut spacing = ops
    .split(['+', '*'])
    .skip(1)
    .map(|space| space.len())
    .collect_vec();

  *spacing
    .last_mut()
    .ok_or_else(|| AocError::Parse("Unexpected empty ops line".to_owned()))? += 1;

  Ok((
    strs
      .into_iter()
      .map(|line| {
        spacing
          .iter()
          .scan(0, |offset, space| {
            let result = line[*offset..*offset + *space].to_owned();
            *offset += space + 1;
            Some(result)
          })
          .collect()
      })
      .collect(),
    ops
      .split_ascii_whitespace()
      .map(|op| op.parse())
      .collect::<Result<_, _>>()?,
  ))
}

fn parse_p1(inputs: &[Vec<String>]) -> AocResult<Vec<Vec<u64>>> {
  inputs.iter().try_fold(
    vec![Vec::new(); inputs[0].len()],
    |mut v, line| -> AocResult<_> {
      for (vec, el) in v.iter_mut().zip(line) {
        vec.push(
          el.trim()
            .parse()
            .map_err(|err| AocError::Parse(format!("Failed to parse \"{el}\" as u64: {err}")))?,
        );
      }
      Ok(v)
    },
  )
}

fn parse_p2(inputs: &[Vec<String>]) -> AocResult<Vec<Vec<u64>>> {
  let res = inputs.iter().fold(
    inputs[0]
      .iter()
      .map(|example_input| vec!["".to_owned(); example_input.len()])
      .collect_vec(),
    |mut v, line| {
      for (vec, el) in v.iter_mut().zip(line) {
        for (s, c) in vec.iter_mut().zip(el.chars()) {
          s.push(c);
        }
      }
      v
    },
  );

  Ok(
    res
      .into_iter()
      .map(|line| line.into_iter().map(|el| el.trim().parse()).collect())
      .collect::<Result<_, _>>()?,
  )
}

pub struct P6;

impl NumericSolution for P6 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let (inputs, ops) = build_grid(input_path)?;

    let inputs = match part {
      Part::P1 => parse_p1(&inputs)?,
      Part::P2 => parse_p2(&inputs)?,
    };

    Ok(
      ops
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
