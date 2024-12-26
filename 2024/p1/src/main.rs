use util::{error::AocResult, parse::two_lists};

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let (mut v1, mut v2) = two_lists::<i32>(INPUT_FILE)?;

  v1.sort();
  v2.sort();

  let sum_of_diffs = v1
    .iter()
    .zip(v2.iter())
    .map(|(&e1, &e2)| (e1 - e2).abs())
    .sum::<i32>();

  println!("Sum of diffs: {sum_of_diffs}");

  Ok(())
}
