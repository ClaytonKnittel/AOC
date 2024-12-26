use util::{algorithm::frequency_map, error::AocResult, parse::two_lists};

fn sum_of_diffs(v1: &Vec<i32>, v2: &Vec<i32>) -> i32 {
  v1.iter()
    .zip(v2.iter())
    .map(|(&e1, &e2)| (e1 - e2).abs())
    .sum::<i32>()
}

fn similarity_score(v1: &Vec<i32>, v2: &Vec<i32>) -> i32 {
  let s1 = frequency_map(v1);
  let s2 = frequency_map(v2);
  s1.iter()
    .map(|(&elem, &freq)| elem * (freq as i32) * (*s2.get(&elem).unwrap_or(&0) as i32))
    .sum::<i32>()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let (mut v1, mut v2) = two_lists::<i32>(INPUT_FILE)?;

  v1.sort();
  v2.sort();

  println!("Sum of diffs: {}", sum_of_diffs(&v1, &v2));
  println!("Similarity score: {}", similarity_score(&v1, &v2));

  Ok(())
}
