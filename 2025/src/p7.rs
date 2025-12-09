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

    let (beams, splits) = iter.try_fold(
      (vec![(start, 1)], 0),
      |(beams, mut splits), line| -> AocResult<_> {
        let line = line?;

        let mut next_beams = Vec::new();
        for (beam, paths) in beams {
          if line.as_bytes()[beam] == b'^' {
            splits += 1;
            match next_beams.last_mut() {
              Some((v, p)) => {
                if *v == beam - 1 {
                  *p += paths;
                } else {
                  next_beams.push((beam - 1, paths));
                }
              }
              None => next_beams.push((beam - 1, paths)),
            }
            next_beams.push((beam + 1, paths));
          } else {
            match next_beams.last_mut() {
              Some((v, p)) => {
                if *v == beam {
                  *p += paths;
                } else {
                  next_beams.push((beam, paths));
                }
              }
              None => next_beams.push((beam, paths)),
            }
          }
        }

        Ok((next_beams, splits))
      },
    )?;

    match part {
      Part::P1 => Ok(splits),
      Part::P2 => Ok(beams.iter().map(|(_, paths)| paths).sum()),
    }
  }
}
