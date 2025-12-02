use std::{fs::read_to_string, str::FromStr};

use util::error::{AocError, AocResult};

fn sum_of_bad_ids_above(num: u64) -> u64 {
  debug_assert!(num.ilog10() % 2 == 1);
  let digits = num.ilog10() + 1;
  let div = 10_u64.pow(digits / 2);
  let hi = num / div;
  let lo = num % div;

  let mut sum = div * (div - 1) / 2 - hi * (hi + 1) / 2;
  if hi >= lo {
    sum += hi;
  }
  sum * (div + 1)
}

fn sum_of_bad_ids_below(num: u64) -> u64 {
  debug_assert!(num.ilog10() % 2 == 1);
  let digits = num.ilog10() + 1;
  let div = 10_u64.pow(digits / 2);
  let hi = num / div;
  let lo = num % div;

  let min = div / 10;
  let mut sum = hi * (hi - 1) / 2 - min * min.wrapping_sub(1) / 2;
  if hi <= lo {
    sum += hi;
  }
  sum * (div + 1)
}

struct Range {
  low: u64,
  high: u64,
}

impl Range {
  fn bad_id_count(&self) -> u64 {
    let lo_digits = self.low.ilog10() + 1;
    let hi_digits = self.high.ilog10() + 1;
    if lo_digits == hi_digits {
      if lo_digits % 2 == 0 {
        sum_of_bad_ids_above(self.low as u64) - sum_of_bad_ids_above((self.high + 1) as u64)
      } else {
        0
      }
    } else {
      let mut sum = 0;
      if lo_digits % 2 == 0 {
        sum += sum_of_bad_ids_above(self.low as u64);
      }

      for digits in ((lo_digits + 1) & !1)..((hi_digits - 1) & !1) {
        sum += sum_of_bad_ids_above(10_u64.pow(digits - 1));
      }

      if hi_digits % 2 == 0 {
        sum += sum_of_bad_ids_below(self.high as u64);
      }

      sum
    }
  }
}

impl FromStr for Range {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, AocError> {
    let (low, high) = s
      .split_once('-')
      .ok_or_else(|| AocError::Parse("Expected '-' in \"{s}\"".to_owned()))?;
    Ok(Self {
      low: low
        .parse()
        .map_err(|err| AocError::Parse(format!("Failed to parse low \"{low}\": {err}")))?,
      high: high
        .parse()
        .map_err(|err| AocError::Parse(format!("Failed to parse high \"{high}\": {err}")))?,
    })
  }
}

fn main() -> AocResult {
  const INPUT: &str = "input.txt";
  let bad_id_count = read_to_string(INPUT)?
    .trim()
    .split(',')
    .map(|range| range.parse::<Range>())
    .try_fold(0, |bad_id_count, range| -> AocResult<_> {
      Ok(bad_id_count + range?.bad_id_count())
    })?;

  println!("Bad ID count: {bad_id_count}");

  Ok(())
}
