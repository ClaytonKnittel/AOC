use std::{fs::read_to_string, str::FromStr};

use itertools::Itertools;
use sieve_rs::PrimeFactorSieve;
use util::error::{AocError, AocResult};

use crate::solution::{NumericSolution, Part};

struct DigitCounts {
  count: u32,
  positive: bool,
}

fn digit_counts(digits: u32, sieve: &PrimeFactorSieve) -> impl Iterator<Item = DigitCounts> {
  sieve
    .prime_factors(digits)
    .map(|(p, _)| p)
    .powerset()
    .filter(|primes| !primes.is_empty())
    .map(|primes| DigitCounts {
      count: primes.iter().product(),
      positive: primes.len() % 2 == 1,
    })
}

fn sum_of_bad_ids_between(lo: u64, hi: u64, count: u32) -> u64 {
  let digits = lo.ilog10() + 1;
  debug_assert!(
    digits.is_multiple_of(count),
    "{lo} log10 == {digits}, vs {count}"
  );
  debug_assert!(lo.ilog10() == hi.ilog10());
  debug_assert!(lo <= hi);

  let div = 10_u64.pow(digits / count * (count - 1));
  let lo_hi = lo / div;
  let lo_lo = lo % div;
  let hi_hi = hi / div;
  let hi_lo = hi % div;

  let pow10_above = 10_u64.pow(digits / count);
  let splayed = (0..(count - 1)).fold(0, |s, _| pow10_above * s + 1);

  let mut sum = (hi_hi * (hi_hi - 1) / 2).wrapping_sub(lo_hi * (lo_hi + 1) / 2);
  if lo_hi * splayed >= lo_lo {
    sum = sum.wrapping_add(lo_hi);
  }
  if hi_hi * splayed <= hi_lo {
    sum = sum.wrapping_add(hi_hi);
  }
  sum * (splayed * pow10_above + 1)
}

struct Range {
  low: u64,
  high: u64,
}

impl Range {
  fn bad_id_count(&self, sieve: &PrimeFactorSieve, p2: bool) -> u64 {
    let lo_digits = self.low.ilog10() + 1;
    let hi_digits = self.high.ilog10() + 1;

    if p2 {
      (lo_digits..=hi_digits)
        .flat_map(|digits| digit_counts(digits, sieve).map(move |counts| (digits, counts)))
        .map(|(digits, DigitCounts { count, positive })| {
          let lo = self.low.max(10_u64.pow(digits - 1));
          let hi = self.high.min(10_u64.pow(digits) - 1);
          let sum = sum_of_bad_ids_between(lo, hi, count);
          if positive {
            sum as i64
          } else {
            -(sum as i64)
          }
        })
        .sum::<i64>() as u64
    } else {
      (lo_digits..=hi_digits)
        .filter(|digits| digits.is_multiple_of(2))
        .map(|digits| {
          let lo = self.low.max(10_u64.pow(digits - 1));
          let hi = self.high.min(10_u64.pow(digits) - 1);
          sum_of_bad_ids_between(lo, hi, 2)
        })
        .sum()
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

pub struct P2;

impl NumericSolution for P2 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let sieve = PrimeFactorSieve::new(100);

    read_to_string(input_path)?
      .trim()
      .split(',')
      .map(|range| range.parse::<Range>())
      .try_fold(0, |count, range| -> AocResult<_> {
        Ok(count + range?.bad_id_count(&sieve, part.p2()))
      })
  }
}
