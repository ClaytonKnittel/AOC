use util::{error::AocResult, parse::list_of_strings};

fn main() -> AocResult {
  let (zero_count, _) = list_of_strings("input.txt")?.try_fold(
    (0, 50),
    |(zero_count, tick), line| -> AocResult<_> {
      let line = line?;
      let (dir, count) = line.split_at(1);
      let count: u32 = count.parse()?;
      let count = count % 100;
      if dir == "L" {
        let tick = (tick + 100 - count) % 100;
        Ok((zero_count + if tick == 0 { 1 } else { 0 }, tick))
      } else {
        let tick = (tick + count) % 100;
        Ok((zero_count + if tick == 0 { 1 } else { 0 }, tick))
      }
    },
  )?;

  println!("Result: {zero_count}");

  Ok(())
}
