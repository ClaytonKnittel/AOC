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

fn compute_checksum_no_frag(layout: &[u8]) -> u64 {
  let mut free_spaces = layout
    .iter()
    .step_by(2)
    .zip(layout.iter().skip(1).step_by(2))
    .map(|(&block_size, &free_space)| (read_size(block_size), read_size(free_space)))
    .scan(0, |offset, (block_size, free_space)| {
      let cur_offset = *offset + block_size;
      *offset = cur_offset + free_space;
      Some((cur_offset, free_space))
    })
    .collect::<Vec<_>>();

  // 9 unique size classes (1-9), each element is an index in free_spaces to start searching for free space.
  let mut start_from = [0; 9];
  let mut checksum = 0;
  for block_idx in (0..layout.len()).step_by(2).rev() {
    let id = block_idx / 2;
    let size = read_size(layout[block_idx]);
    let offset = if id != 0 {
      let (offset, space) = free_spaces[id - 1];
      offset + space
    } else {
      0
    };

    // Find space for block
    checksum += (start_from[size - 1]..id)
      .find_map(|search_idx| {
        let (space_offset, space_size) = free_spaces[search_idx];
        if space_size >= size {
          let checksum = checksum_range(space_offset, id, size);
          free_spaces[search_idx] = (space_offset + size, space_size - size);
          Some(checksum)
        } else {
          start_from[size - 1] = search_idx + 1;
          None
        }
      })
      .unwrap_or_else(|| checksum_range(offset, id, size));
  }

  checksum
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut layout = Vec::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_end(&mut layout)?;

  let checksum = compute_checksum(&layout);
  println!("Checksum: {checksum}");

  let checksum_no_frag = compute_checksum_no_frag(&layout);
  println!("Checksum w/o fragmentation: {checksum_no_frag}");

  Ok(())
}
