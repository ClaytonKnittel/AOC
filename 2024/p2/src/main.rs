use util::{error::AocResult, parse::list_of_lists};

fn is_safe(report: &[u32]) -> bool {
  let (all_desc, all_asc, within_range) = report
    .iter()
    .zip(report.iter().skip(1))
    .map(|(&e1, &e2)| e2 as i32 - e1 as i32)
    .fold(
      (true, true, true),
      |(all_desc, all_asc, within_range), diff| {
        (
          all_desc && diff < 0,
          all_asc && diff > 0,
          within_range && diff.abs() <= 3,
        )
      },
    );

  (all_desc || all_asc) && within_range
}

fn how_many_safe(reports: &[Vec<u32>]) -> u32 {
  reports.iter().filter(|&report| is_safe(report)).count() as u32
}

fn how_many_dampener(reports: &[Vec<u32>]) -> u32 {
  reports
    .iter()
    .filter(|&report| {
      (0..report.len()).any(|idx| {
        let (l, r) = report.split_at(idx);
        let mut v = l.to_vec();
        v.append(&mut r[1..].to_vec());
        is_safe(&v)
      })
    })
    .count() as u32
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let reports = list_of_lists::<u32>(INPUT_FILE)?;

  println!("How many safe reports? {}", how_many_safe(&reports));
  println!(
    "How many safe reports with dampener? {}",
    how_many_dampener(&reports)
  );

  Ok(())
}
