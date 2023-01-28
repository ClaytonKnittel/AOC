fn char_to_idx(c: u32) -> u32 {
  let h1 = c & 0x1fu32;
  let is_lowercase = c & 0x20u32;

  let offset = if is_lowercase == 0 { 26 } else { 0 };

  h1 + offset - 1
}

fn rucksack_mask(rucksack: &str) -> u64 {
  rucksack.chars().fold(0u64, |mask, c| {
    let offset = char_to_idx(c as u32);
    mask | (1u64 << offset)
  })
}

fn item_prio(r1: &str, r2: &str, r3: &str) -> u32 {
  let s1 = rucksack_mask(r1);
  let s2 = rucksack_mask(r2);
  let s3 = rucksack_mask(r3);

  let intersect = s1 & s2 & s3;
  intersect.trailing_zeros() + 1
}

fn main() -> Result<(), std::io::Error> {
  let contents: Vec<String> = utils::get_input()?
    .into_iter()
    .collect::<Result<Vec<String>, std::io::Error>>()?;

  let mut iter = itertools::izip!(
    contents.iter().step_by(3),
    contents.iter().skip(1).step_by(3),
    contents.iter().skip(2).step_by(3)
  );

  let prio_sum = iter.try_fold(0u32, |prio_sum, (r1, r2, r3)| {
    Ok::<u32, std::io::Error>(prio_sum + item_prio(&r1, &r2, &r3))
  })?;

  println!("Sum of item priorities: {}", prio_sum);
  Ok(())
}
