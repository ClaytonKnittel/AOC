use std::{
  fs::File,
  io::{BufReader, Read},
};

use util::{error::AocResult, math::digit_count, parse::parse_list};

fn count_splits(stone: u64, depth: u32) -> u64 {
  if depth == 0 {
    return 1;
  }

  if stone == 0 {
    count_splits(1, depth - 1)
  } else {
    let digits = digit_count(stone);
    if digits % 2 == 0 {
      let divisor = 10u64.pow(digits / 2);
      count_splits(stone % divisor, depth - 1) + count_splits(stone / divisor, depth - 1)
    } else {
      count_splits(stone * 2024, depth - 1)
    }
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut line = String::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_string(&mut line)?;
  let stones = parse_list::<u64>(&line)?;

  let total_stones = stones
    .iter()
    .map(|&stone| count_splits(stone, 25))
    .sum::<u64>();
  println!("Total stones after 25 splits: {total_stones}");

  Ok(())
}
