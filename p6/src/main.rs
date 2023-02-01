struct UniqueStream {
  chars: Vec<Option<char>>,
  next_idx: usize,
}

impl UniqueStream {
  pub fn new(seq_len: u32) -> Self {
    Self {
      chars: vec![None; seq_len as usize - 1],
      next_idx: 0,
    }
  }

  pub fn push(&mut self, c: char) -> bool {
    let mut all_unique = true;
    for idx in 0..self.chars.len() {
      let o = self.chars[idx];
      if o == Some(c) || o == None {
        self.chars[idx] = None;
        all_unique = false;
      }
    }

    self.chars[self.next_idx] = Some(c);
    self.next_idx = if self.next_idx == self.chars.len() - 1 {
      0
    } else {
      self.next_idx + 1
    };
    return all_unique;
  }
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::all_input()?;
  let line = contents.remove(0);

  let n_in_row = 4;

  let mut us = UniqueStream::new(n_in_row);
  let idx_char_opt = line.chars().enumerate().find(|(_1, c)| us.push(*c));

  match idx_char_opt {
    Some((idx, _2)) => {
      println!(
        "Found unique set of {} chars, start at {}",
        n_in_row,
        idx + 1
      );
    }
    None => {
      println!("No unique strings found of such length!\n");
    }
  }

  Ok(())
}
