use util::{error::AocResult, parse::list_of_strings};

use crate::solution::{NumericSolution, Part};

pub struct P1;

impl NumericSolution for P1 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    Ok(
      list_of_strings(input_path)?
        .try_fold((0, 50), |(zero_count, tick), line| -> AocResult<_> {
          let line = line?;
          let (dir, count) = line.split_at(1);
          let count: u64 = count.parse()?;
          let zero_count = zero_count + part.p2() as u64 * count / 100;
          let count = count % 100;
          if dir == "L" {
            let new_tick = (tick + 100 - count) % 100;
            Ok((
              zero_count + (new_tick == 0 || (part.p2() && tick != 0 && new_tick > tick)) as u64,
              new_tick,
            ))
          } else {
            let new_tick = (tick + count) % 100;
            Ok((
              zero_count + (new_tick == 0 || (part.p2() && new_tick < tick)) as u64,
              new_tick,
            ))
          }
        })?
        .0,
    )
  }
}
