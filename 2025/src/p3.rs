use itertools::Itertools;
use util::{error::AocResult, parse::list_of_strings};

use crate::solution::{NumericSolution, Part};

fn max_joltage(line: &str) -> u64 {
  let (h, l) = line
    .chars()
    .map(|c| (c as u8 - b'0') as u64)
    .tuple_windows()
    .fold(
      (0, 0),
      |(h, l), (hv, lv)| {
        if hv > h {
          (hv, lv)
        } else {
          (h, l.max(lv))
        }
      },
    );
  h * 10 + l
}

pub struct P3;

impl NumericSolution for P3 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    list_of_strings(input_path)?.try_fold(0, |sum, line| Ok(sum + max_joltage(&line?)))
  }
}
