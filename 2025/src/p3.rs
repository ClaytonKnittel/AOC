use itertools::Itertools;
use util::{error::AocResult, parse::list_of_strings};

use crate::solution::{NumericSolution, Part};

fn max_joltage1(line: &str) -> u64 {
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

fn max_joltage2<const N: usize>(line: &str) -> u64 {
  line
    .chars()
    .map(|c| (c as u8 - b'0') as u64)
    .enumerate()
    .fold([0; N], |mut v, (idx, c)| {
      if v[N - 1] >= c {
        return v;
      }

      v[N - 1] = c;
      for i in (idx.saturating_sub(line.len().saturating_sub(N))..N - 1).rev() {
        if v[i] < c {
          v[i] = c;
          v[i + 1] = 0;
        } else {
          break;
        }
      }
      v
    })
    .into_iter()
    .fold(0, |acc, v| 10 * acc + v)
}

fn max_joltage(line: &str, part: Part) -> u64 {
  match part {
    Part::P1 => max_joltage1(line),
    Part::P2 => max_joltage2::<12>(line),
  }
}

pub struct P3;

impl NumericSolution for P3 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    list_of_strings(input_path)?.try_fold(0, |sum, line| Ok(sum + max_joltage(&line?, part)))
  }
}
