use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

pub struct P7;

impl NumericSolution for P7 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let mut iter = list_of_strings(input_path)?;
    let start_line = iter
      .next()
      .ok_or_else(|| AocError::Parse("Unexpected empty input".to_owned()))??;

    let start = start_line
      .find('S')
      .ok_or_else(|| AocError::Parse("'S' not found in first line".to_owned()))?;

    let (_v, splits) =
      iter.try_fold((vec![start], 0), |(v, mut splits), line| -> AocResult<_> {
        let line = line?;

        let mut next_v = Vec::new();
        for beam in v {
          if line.as_bytes()[beam] == b'^' {
            splits += 1;
            if next_v.last() != Some(&(beam - 1)) {
              next_v.push(beam - 1);
            }
            next_v.push(beam + 1);
          } else if next_v.last() != Some(&beam) {
            next_v.push(beam);
          }
        }

        Ok((next_v, splits))
      })?;

    Ok(splits)
  }
}
