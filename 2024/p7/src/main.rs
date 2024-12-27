use std::{
  fs::File,
  io::{BufRead, BufReader},
};

use util::{error::AocResult, math::digit_count, parse::parse_list};

struct Equation {
  result: u64,
  inputs: Vec<u64>,
}

impl Equation {
  fn concat(l: u64, r: u64) -> u64 {
    l * 10u64.pow(digit_count(r)) + r
  }

  fn solve_helper(result: u64, current: u64, inputs: &[u64]) -> bool {
    if inputs.is_empty() {
      result == current
    } else {
      let next_input = inputs[0];
      Self::solve_helper(result, current + next_input, &inputs[1..])
        || Self::solve_helper(result, current * next_input, &inputs[1..])
    }
  }

  fn solveable(&self) -> bool {
    Self::solve_helper(self.result, self.inputs[0], &self.inputs[1..])
  }

  fn solve2_helper(result: u64, current: u64, inputs: &[u64]) -> bool {
    if inputs.is_empty() {
      result == current
    } else {
      let next_input = inputs[0];
      Self::solve2_helper(result, current + next_input, &inputs[1..])
        || Self::solve2_helper(result, current * next_input, &inputs[1..])
        || Self::solve2_helper(result, Self::concat(current, next_input), &inputs[1..])
    }
  }

  fn solveable2(&self) -> bool {
    Self::solve2_helper(self.result, self.inputs[0], &self.inputs[1..])
  }
}

fn parse_input(input_file: &str) -> AocResult<Vec<Equation>> {
  BufReader::new(File::open(input_file)?)
    .lines()
    .map(|line| -> AocResult<_> {
      let line = line?;
      let (result_str, inputs_str) = line.split_once(": ").unwrap();
      Ok(Equation {
        result: result_str.parse::<u64>()?,
        inputs: parse_list(inputs_str)?,
      })
    })
    .collect()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let equations = parse_input(INPUT_FILE)?;

  let num_solvable = equations
    .iter()
    .map(|equation| {
      if equation.solveable() {
        equation.result
      } else {
        0
      }
    })
    .sum::<u64>();
  println!("Num solvable: {num_solvable}");

  let num_solvable2 = equations
    .iter()
    .map(|equation| {
      if equation.solveable2() {
        equation.result
      } else {
        0
      }
    })
    .sum::<u64>();
  println!("Num solvable with ||: {num_solvable2}");

  Ok(())
}

#[cfg(test)]
mod test {
  use crate::Equation;

  #[test]
  fn test_concat() {
    assert_eq!(Equation::concat(17, 8), 178);
  }
  #[test]
  fn test_concat2() {
    assert_eq!(Equation::concat(178, 28), 17828);
  }
}
