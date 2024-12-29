use std::fs::read_to_string;

use util::error::{AocError, AocResult};

fn possible(pattern: &[u8], towels: &[&[u8]]) -> bool {
  let mut m = vec![false; pattern.len() + 1];
  m[pattern.len()] = true;

  (0..pattern.len()).rev().fold(m, |mut m, from_idx| {
    let to_cover = &pattern[from_idx..];
    m[from_idx] = towels.iter().any(|&towel| {
      towel.len() <= to_cover.len()
        && m[from_idx + towel.len()]
        && towel == &to_cover[..towel.len()]
    });
    m
  })[0]
}

fn ways(pattern: &[u8], towels: &[&[u8]]) -> u64 {
  let mut m = vec![0; pattern.len() + 1];
  m[pattern.len()] = 1;

  (0..pattern.len()).rev().fold(m, |mut m, from_idx| {
    let to_cover = &pattern[from_idx..];
    m[from_idx] = towels
      .iter()
      .map(|&towel| {
        if towel.len() <= to_cover.len() && towel == &to_cover[..towel.len()] {
          m[from_idx + towel.len()]
        } else {
          0
        }
      })
      .sum();
    m
  })[0]
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let contents = read_to_string(INPUT_FILE)?;
  let (towels_str, patterns_str) = contents
    .split_once("\n\n")
    .ok_or_else(|| AocError::Parse("No blank line found in input".to_owned()))?;

  let towels: Vec<_> = towels_str
    .split(", ")
    .map(|towel| towel.as_bytes())
    .collect();

  let patterns: Vec<_> = patterns_str.lines().map(|line| line.as_bytes()).collect();

  let num_possible_patterns = patterns
    .iter()
    .filter(|pattern| possible(pattern, &towels))
    .count();
  println!("Num possible patterns: {num_possible_patterns}");

  let sum_possible_ways: u64 = patterns.iter().map(|pattern| ways(pattern, &towels)).sum();
  println!("Sum of possible ways: {sum_possible_ways}");

  Ok(())
}
