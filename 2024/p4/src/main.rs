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

fn is_x_mas(letters: &[[char; 3]; 3]) -> bool {
  let c1 = letters[0][0];
  let c2 = letters[0][2];
  let c3 = letters[2][0];
  let c4 = letters[2][2];

  let center = letters[1][1];

  center == 'A'
    && c1 != c4
    && ((c1 == c2 && c3 == c4) || (c1 == c3 && c2 == c4))
    && (c1 == 'M' || c4 == 'M')
    && (c1 == 'S' || c4 == 'S')
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

  let x_mas = (0..(n as usize - 2))
    .flat_map(|row_idx| {
      let grid = &grid;
      (0..(m as usize - 2)).filter(move |&col_idx| {
        let grid = [
          [
            grid[row_idx][col_idx],
            grid[row_idx][col_idx + 1],
            grid[row_idx][col_idx + 2],
          ],
          [
            grid[row_idx + 1][col_idx],
            grid[row_idx + 1][col_idx + 1],
            grid[row_idx + 1][col_idx + 2],
          ],
          [
            grid[row_idx + 2][col_idx],
            grid[row_idx + 2][col_idx + 1],
            grid[row_idx + 2][col_idx + 2],
          ],
        ];
        is_x_mas(&grid)
      })
    })
    .count();

  println!("{}", x_mas);

  Ok(())
}
