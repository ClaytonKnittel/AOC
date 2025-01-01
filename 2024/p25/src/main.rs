use std::{error::Error, fs::read_to_string, str::FromStr};

use util::error::{AocError, AocResult};

const MAX_HEIGHT: u8 = 5;

fn parse_heights<'a>(lines: impl Iterator<Item = &'a str>) -> AocResult<[u8; 5]> {
  lines
    .enumerate()
    .try_fold([0; 5], |mut heights, (row, line)| {
      if line.len() != 5 {
        return Err(AocError::Parse(format!("Line \"{line}\" not length 5 for Lock")).into());
      }

      for (col, c) in line.chars().enumerate() {
        if c == '#' {
          heights[col] = row as u8;
        }
      }

      Ok(heights)
    })
    .and_then(|heights| {
      if heights.iter().all(|&height| height <= MAX_HEIGHT) {
        Ok(heights)
      } else {
        Err(AocError::Parse("All heights are 6".to_owned()).into())
      }
    })
}

#[derive(Debug)]
enum Diagram {
  Lock(Lock),
  Key(Key),
}

impl FromStr for Diagram {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if let Ok(lock) = s.parse() {
      Ok(Self::Lock(lock))
    } else if let Ok(key) = s.parse() {
      Ok(Self::Key(key))
    } else {
      Err(AocError::Parse("Could not parse as lock or key".to_owned()).into())
    }
  }
}

#[derive(Debug)]
struct Lock {
  heights: [u8; 5],
}

impl FromStr for Lock {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(Self {
      heights: parse_heights(s.lines())?,
    })
  }
}

#[derive(Debug)]
struct Key {
  heights: [u8; 5],
}

impl Key {
  fn fits(&self, lock: &Lock) -> bool {
    self
      .heights
      .iter()
      .zip(lock.heights.iter())
      .all(|(&key_height, &lock_height)| key_height + lock_height <= MAX_HEIGHT)
  }
}

impl FromStr for Key {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(Self {
      heights: parse_heights(s.lines().rev())?,
    })
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";

  let (locks, keys) = read_to_string(INPUT_FILE)?
    .split("\n\n")
    .map(|diagram| diagram.parse())
    .try_fold(
      (vec![], vec![]),
      |(mut locks, mut keys), diagram| -> AocResult<_> {
        match diagram? {
          Diagram::Lock(lock) => {
            locks.push(lock);
          }
          Diagram::Key(key) => {
            keys.push(key);
          }
        }
        Ok((locks, keys))
      },
    )?;

  let fitting_pairs = keys
    .iter()
    .flat_map(|key| locks.iter().filter(|lock| key.fits(lock)))
    .count();
  println!("Fitting pairs: {fitting_pairs}");

  Ok(())
}
