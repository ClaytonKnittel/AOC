fn char_to_idx(c: u32) -> u32 {
  let h1 = c & 0x1fu32;
  let is_lowercase = c & 0x20u32;

  let offset = if is_lowercase == 0 { 26 } else { 0 };

  h1 + offset - 1
}

fn item_prio(rucksack: &str) -> u32 {
  let mut s1 = 0u64;
  let mut s2 = 0u64;

  for c in rucksack[..rucksack.len() / 2].chars() {
    let offset = char_to_idx(c as u32);
    s1 |= 1u64 << offset;
  }
  for c in rucksack[rucksack.len() / 2..].chars() {
    let offset = char_to_idx(c as u32);
    s2 |= 1u64 << offset;
  }

  let intersect = s1 & s2;
  intersect.trailing_zeros() + 1
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?;

  let prio_sum = contents.try_fold(0u32, |prio_sum, rucksack| {
    Ok::<u32, std::io::Error>(prio_sum + item_prio(&rucksack?))
  })?;

  println!("Sum of item priorities: {}", prio_sum);
  Ok(())
}
