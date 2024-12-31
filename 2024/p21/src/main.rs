use std::{
  error::Error,
  fmt::{self, Debug, Display, Formatter},
  fs::read_to_string,
  str::FromStr,
};

use util::{
  error::{AocError, AocResult},
  grid::Pos,
};

trait Button: Clone + Copy + PartialEq + Eq {
  fn pos(&self) -> Pos;

  fn path(&self, to: Self) -> Vec<(RobotButton, u32)> {
    if *self == to {
      return vec![];
    }

    let to_travel = to.pos() - self.pos();
    let lr = if to_travel.dc > 0 {
      RobotButton::Right
    } else {
      RobotButton::Left
    };
    let ud = if to_travel.dr > 0 {
      RobotButton::Up
    } else {
      RobotButton::Down
    };
    let dc = to_travel.dc.unsigned_abs() as u32;
    let dr = to_travel.dr.unsigned_abs() as u32;

    [
      (to_travel.dc != 0).then_some((lr, dc)),
      (to_travel.dr != 0).then_some((ud, dr)),
      Some((RobotButton::A, 1)),
    ]
    .into_iter()
    .flatten()
    .collect()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RobotButton {
  Up,
  Right,
  Down,
  Left,
  A,
}

impl Button for RobotButton {
  fn pos(&self) -> Pos {
    match self {
      Self::Up => Pos { col: 1, row: 1 },
      Self::Right => Pos { col: 2, row: 0 },
      Self::Down => Pos { col: 1, row: 0 },
      Self::Left => Pos { col: 0, row: 0 },
      Self::A => Pos { col: 2, row: 1 },
    }
  }
}

impl Display for RobotButton {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Up => '^',
        Self::Right => '>',
        Self::Down => 'v',
        Self::Left => '<',
        Self::A => 'A',
      }
    )
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keypad {
  Number(u8),
  A,
}

impl Button for Keypad {
  fn pos(&self) -> Pos {
    match self {
      Keypad::Number(0) => Pos { col: 1, row: 0 },
      Keypad::Number(num) => Pos {
        col: ((num - 1) % 3) as isize,
        row: 1 + ((num - 1) / 3) as isize,
      },
      Keypad::A => Pos { col: 2, row: 0 },
    }
  }
}

impl FromStr for Keypad {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s.len() != 1 {
      return Err(AocError::Parse(format!("{s} not a Keypad button")).into());
    }

    match s.as_bytes()[0] {
      b'A' => Ok(Keypad::A),
      digit @ b'0'..=b'9' => Ok(Keypad::Number(digit - b'0')),
      _ => Err(AocError::Parse(format!("{s} not recognized as a Keypad button")).into()),
    }
  }
}

impl Display for Keypad {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Number(digit) => (b'0' + digit) as char,
        Self::A => 'A',
      }
    )
  }
}

/// Measures cost of moving from button `cur_button` to `to_push`, then pushing
/// `to_push`. This assumes all other robots before this one are already
/// hovering over A.
fn push_cost<B: Button + Display>(cur_button: B, to_push: B, depth: usize) -> u64 {
  if depth == 0 {
    print!("{to_push}");
    return 1;
  }

  cur_button
    .path(to_push)
    .into_iter()
    .scan(RobotButton::A, |from, (to, distance)| {
      let cost = push_cost(*from, to, depth - 1) + distance as u64 - 1;
      *from = to;
      Some(cost)
    })
    .sum()
}

fn sequence_cost(code: &[Keypad], depth: usize) -> u64 {
  code
    .iter()
    .scan(Keypad::A, |from, &to| {
      let cost = push_cost(*from, to, depth);
      *from = to;
      Some(cost)
    })
    .sum()
}

fn complexity(code: &[Keypad], depth: usize) -> u64 {
  sequence_cost(code, depth)
    * code
      .iter()
      .filter_map(|key| match key {
        Keypad::A => None,
        Keypad::Number(digit) => Some(digit),
      })
      .fold(0, |total, &digit| total * 10 + digit as u64)
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input2.txt";
  const DEPTH: usize = 3;

  let codes: Vec<Vec<_>> = read_to_string(INPUT_FILE)?
    .lines()
    .map(|line| {
      line
        .chars()
        .map(|c| c.to_string().parse())
        .collect::<AocResult<_>>()
    })
    .collect::<AocResult<_>>()?;

  let complexity: u64 = codes.iter().map(|code| complexity(code, DEPTH)).sum();
  println!("Total complexity {complexity}");

  Ok(())
}
