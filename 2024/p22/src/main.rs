use std::fs::read_to_string;

use util::error::AocResult;

fn next(secret: u64) -> u64 {
  fn mix(s: u64, v: u64) -> u64 {
    s ^ v
  }
  fn prune(s: u64) -> u64 {
    s % 16777216
  }

  let a = prune(mix(secret * 64, secret));
  let b = prune(mix(a / 32, a));
  prune(mix(b * 2048, b))
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let secrets: Vec<u64> = read_to_string(INPUT_FILE)?
    .lines()
    .map(|line| Ok(line.parse()?))
    .collect::<AocResult<_>>()?;

  let sum_of_2000th_secrets: u64 = secrets
    .iter()
    .map(|&secret| (0..2000).fold(secret, |secret, _| next(secret)))
    .sum();
  println!("Sum of 2000th secrets: {sum_of_2000th_secrets}");

  Ok(())
}
