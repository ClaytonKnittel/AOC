use std::{
  fs::File,
  io::{BufReader, Read},
};

use util::error::AocResult;

fn read_size(encoding: u8) -> usize {
  (encoding - b'0') as usize
}

fn checksum_range(start_offset: usize, id: usize, size: usize) -> u64 {
  ((start_offset * size + (size * (size.wrapping_sub(1)) / 2)) * id) as u64
}

fn compute_checksum(layout: &[u8]) -> u64 {
  let mut checksum = 0;
  let mut start_pos = 0;
  let mut cur_offset = 0;
  let mut end_pos = layout.len() - 1;
  let mut end_remaining = read_size(layout[end_pos]);

  while start_pos < end_pos {
    let id = start_pos / 2;
    let size = read_size(layout[start_pos]);
    checksum += checksum_range(cur_offset, id, size);
    cur_offset += size;

    let mut gap_size = read_size(layout[start_pos + 1]);
    while gap_size != 0 && start_pos < end_pos {
      let end_id = end_pos / 2;
      let fill_size = end_remaining.min(gap_size);
      checksum += checksum_range(cur_offset, end_id, fill_size);

      cur_offset += fill_size;
      gap_size -= fill_size;
      end_remaining -= fill_size;
      if end_remaining == 0 {
        end_pos -= 2;
        end_remaining = read_size(layout[end_pos]);
      }
    }

    start_pos += 2;
  }

  checksum + checksum_range(cur_offset, end_pos / 2, end_remaining)
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut layout = Vec::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_end(&mut layout)?;

  let checksum = compute_checksum(&layout);
  println!("Checksum: {checksum}");

  Ok(())
}
