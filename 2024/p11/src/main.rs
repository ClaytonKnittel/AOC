use std::{collections::HashMap, fs::read_to_string};

use util::{error::AocResult, math::digit_count, parse::parse_list};

fn count_splits_helper(stone: u64, depth: u32, cache: &mut HashMap<(u64, u32), u64>) -> u64 {
  if depth == 0 {
    return 1;
  }

  if let Some(&count) = cache.get(&(stone, depth)) {
    return count;
  }

  let result = if stone == 0 {
    count_splits_helper(1, depth - 1, cache)
  } else {
    let digits = digit_count(stone);
    if digits % 2 == 0 {
      let divisor = 10u64.pow(digits / 2);
      count_splits_helper(stone % divisor, depth - 1, cache)
        + count_splits_helper(stone / divisor, depth - 1, cache)
    } else {
      count_splits_helper(stone * 2024, depth - 1, cache)
    }
  };

  cache.insert((stone, depth), result);
  result
}

fn count_splits(stones: &[u64], depth: u32) -> u64 {
  let mut cache = HashMap::new();
  stones
    .iter()
    .map(|&stone| count_splits_helper(stone, depth, &mut cache))
    .sum()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let stones = parse_list::<u64>(&read_to_string(INPUT_FILE)?)?;

  let total_stones = count_splits(&stones, 25);
  println!("Total stones after 25 splits: {total_stones}");

  let total_stones = count_splits(&stones, 75);
  println!("Total stones after 75 splits: {total_stones}");

  Ok(())
}
