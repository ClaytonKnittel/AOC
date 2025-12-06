use std::{
  cmp::Ordering,
  collections::BTreeSet,
  ops::Bound::{Excluded, Included, Unbounded},
};

use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

#[derive(PartialEq, Eq, Clone, Copy)]
enum RangeBorder {
  Start(u64),
  End(u64),
}

impl RangeBorder {
  fn val(&self) -> u64 {
    match self {
      Self::Start(val) | Self::End(val) => *val,
    }
  }
}

impl Ord for RangeBorder {
  fn cmp(&self, other: &Self) -> Ordering {
    self.val().cmp(&other.val()).then(match (self, other) {
      (Self::Start(_), Self::End(_)) => Ordering::Less,
      (Self::End(_), Self::Start(_)) => Ordering::Greater,
      (Self::Start(_), Self::Start(_)) | (Self::End(_), Self::End(_)) => Ordering::Equal,
    })
  }
}

impl PartialOrd for RangeBorder {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

pub struct P5;

impl NumericSolution for P5 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let mut lines = list_of_strings(input_path)?;
    let mut ranges = BTreeSet::<RangeBorder>::new();
    loop {
      let line = lines
        .next()
        .ok_or_else(|| AocError::Parse("Unexpected end-of-file before blank line".to_owned()))??;

      if line.is_empty() {
        break;
      }

      let (start, end) = line
        .trim_ascii_end()
        .split_once('-')
        .ok_or_else(|| AocError::Parse(format!("Expected a '-' in line \"{line}\"")))?;
      let start = RangeBorder::Start(start.parse()?);
      let end = RangeBorder::End(end.parse::<u64>()? + 1);

      ranges
        .extract_if((Included(start), Included(end)), |_| true)
        .count();
      let prev = ranges
        .range((Unbounded, Excluded(start)))
        .next_back()
        .cloned();
      let next = ranges.range((Excluded(end), Unbounded)).next().cloned();

      if matches!(prev, Some(RangeBorder::End(_)) | None) {
        ranges.insert(start);
      }
      if matches!(next, Some(RangeBorder::Start(_)) | None) {
        ranges.insert(end);
      }
    }

    match part {
      Part::P1 => lines
        .map(|line| -> AocResult<_> { Ok(line?.parse()?) })
        .try_fold(0, |acc, val| {
          val.map(|val| {
            let x = ranges
              .range((Excluded(RangeBorder::End(val)), Unbounded))
              .next();
            if matches!(x, Some(RangeBorder::End(_))) {
              acc + 1
            } else {
              acc
            }
          })
        }),
      Part::P2 => Ok(
        ranges
          .iter()
          .map(|border| match border {
            RangeBorder::Start(val) => -(*val as i64),
            RangeBorder::End(val) => *val as i64,
          })
          .sum::<i64>() as u64,
      ),
    }
  }
}
