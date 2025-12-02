use util::{error::AocResult, parse::list_of_strings};

fn zero_count(file: &str, p2: bool) -> AocResult<u32> {
  Ok(
    list_of_strings(file)?
      .try_fold((0, 50), |(zero_count, tick), line| -> AocResult<_> {
        let line = line?;
        let (dir, count) = line.split_at(1);
        let count: u32 = count.parse()?;
        let zero_count = zero_count + p2 as u32 * count / 100;
        let count = count % 100;
        if dir == "L" {
          let new_tick = (tick + 100 - count) % 100;
          Ok((
            zero_count + (new_tick == 0 || (p2 && tick != 0 && new_tick > tick)) as u32,
            new_tick,
          ))
        } else {
          let new_tick = (tick + count) % 100;
          Ok((
            zero_count + (new_tick == 0 || (p2 && new_tick < tick)) as u32,
            new_tick,
          ))
        }
      })?
      .0,
  )
}

fn main() -> AocResult {
  const FILE: &str = "input.txt";
  println!("Result: {}", zero_count(FILE, false)?);
  println!("Result p2: {}", zero_count(FILE, true)?);

  Ok(())
}
