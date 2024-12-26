use std::borrow::Borrow;

use util::{error::AocResult, parse::list_of_chars};

enum State {
  Any,
  AscM,
  AscA,
  AscS,
  DesA,
  DesM,
  DesX,
}

fn count_xmas<I, C>(letters: I) -> u32
where
  C: Borrow<char>,
  I: IntoIterator<Item = C>,
{
  letters
    .into_iter()
    .fold((State::Any, 0), |(state, mut count), letter| {
      let next_state = match (state, letter.borrow()) {
        (State::AscM, 'M') => State::AscA,
        (State::AscA, 'A') => State::AscS,
        (State::AscS, 'S') => {
          count += 1;
          State::DesA
        }
        (State::DesA, 'A') => State::DesM,
        (State::DesM, 'M') => State::DesX,
        (State::DesX, 'X') => {
          count += 1;
          State::AscM
        }
        (_, 'X') => State::AscM,
        (_, 'S') => State::DesA,
        _ => State::Any,
      };

      (next_state, count)
    })
    .1
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let grid = list_of_chars(INPUT_FILE)?;
  let n = grid.len() as i32;
  let m = grid[0].len() as i32;

  let rows = grid.iter().map(|row| count_xmas(row)).sum::<u32>();
  let cols = (0..m)
    .map(|col_idx| count_xmas((0..n).map(|row_idx| grid[row_idx as usize][col_idx as usize])))
    .sum::<u32>();
  let diag1 = (0..(n + m - 1))
    .map(|diag_idx| {
      count_xmas(
        (0.max(diag_idx - n + 1)..(diag_idx + 1).min(m))
          .map(|diag_iter| grid[(diag_idx - diag_iter) as usize][diag_iter as usize]),
      )
    })
    .sum::<u32>();
  let diag2 = (0..(n + m - 1))
    .map(|diag_idx| {
      count_xmas(
        (0.max(n - diag_idx - 1)..(m + n - diag_idx - 1).min(m))
          .map(|diag_iter| grid[diag_iter as usize][(diag_idx + diag_iter - (n - 1)) as usize]),
      )
    })
    .sum::<u32>();

  println!("{}", rows + cols + diag1 + diag2);

  Ok(())
}
