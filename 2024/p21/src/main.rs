use std::fmt::Debug;

use util::{direction::Direction, error::AocResult, grid::Pos};

trait Button: Clone + Copy + PartialEq + Eq {
  fn pos(&self) -> Pos;

  /// Returns true if it is legal to go left/right, then up/down going from
  /// `self` to `to`.
  fn lr_ud_path_possible(&self, to: Self) -> bool;
  /// Returns true if it is legal to go up/down, then left/right going from
  /// `self` to `to`.
  fn ud_lr_path_possible(&self, to: Self) -> bool;

  fn possible_paths(&self, to: Self) -> Vec<Vec<(Direction, u32)>> {
    if *self == to {
      return vec![vec![]];
    }

    let to_travel = to.pos() - self.pos();
    let lr = if to_travel.dc > 0 {
      Direction::Right
    } else {
      Direction::Left
    };
    let ud: Direction = if to_travel.dr > 0 {
      Direction::Up
    } else {
      Direction::Down
    };
    let dc = to_travel.dc.unsigned_abs() as u32;
    let dr = to_travel.dr.unsigned_abs() as u32;

    [
      (dc != 0 && self.lr_ud_path_possible(to)).then(|| {
        [
          (to_travel.dc != 0).then_some((lr, dc)),
          (to_travel.dr != 0).then_some((ud, dr)),
        ]
        .into_iter()
        .flatten()
        .collect()
      }),
      (dr != 0 && self.ud_lr_path_possible(to)).then(|| {
        [
          (to_travel.dr != 0).then_some((ud, dr)),
          (to_travel.dc != 0).then_some((lr, dc)),
        ]
        .into_iter()
        .flatten()
        .collect()
      }),
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

  fn lr_ud_path_possible(&self, to: Self) -> bool {
    to != Self::Left
  }
  fn ud_lr_path_possible(&self, _to: Self) -> bool {
    *self != Self::Left
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keypad {
  Number(u32),
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

  fn lr_ud_path_possible(&self, to: Self) -> bool {
    self.pos().row != 0 || to.pos().col != 0
  }
  fn ud_lr_path_possible(&self, to: Self) -> bool {
    self.pos().col != 0 || to.pos().row != 0
  }
}

fn count_strokes_helper(
  depth: usize,
  config: &mut [RobotButton],
  to_push: RobotButton,
  code: &[Keypad],
) -> u64 {
  // We are at depth zero and can push any button at will.
  if depth == config.len() {
    return sequence_cost(code[0], config, &code[1..]);
  }

  travel_cost(depth + 1, config[depth], to_push, config, code)
}

fn travel_cost<B: Button + Debug>(
  depth: usize,
  cur_button: B,
  to_push: B,
  config: &mut [RobotButton],
  code: &[Keypad],
) -> u64 {
  let (cost, result_config) = cur_button
    .possible_paths(to_push)
    .into_iter()
    .map(|path| -> (u64, Vec<RobotButton>) {
      println!(
        "Depth {depth}: button {cur_button:?} -> {to_push:?}, path {path:?}, config {config:?}, code {code:?}"
      );
      let mut config_copy: Vec<_> = config.into();
      let cost = path
        .into_iter()
        .map(|(direction, reps)| {
          count_strokes_helper(
            depth,
            &mut config_copy,
            match direction {
              Direction::Up => RobotButton::Up,
              Direction::Right => RobotButton::Right,
              Direction::Down => RobotButton::Down,
              Direction::Left => RobotButton::Left,
            },
            code,
          ) + reps as u64 - 1
        })
        .sum();

      (cost, config_copy)
    })
    .min_by(|(cost1, _), (cost2, _)| cost1.cmp(cost2))
    .unwrap();

  config.copy_from_slice(&result_config);
  cost
}

fn sequence_cost(cur_button: Keypad, config: &mut [RobotButton], code: &[Keypad]) -> u64 {
  if code.is_empty() {
    0
  } else {
    1 + travel_cost(0, cur_button, code[0], config, code)
  }
}

fn count_keypad_strokes(depth: usize, code: &[Keypad]) -> u64 {
  sequence_cost(Keypad::A, &mut vec![RobotButton::A; depth], code)
}

fn main() -> AocResult {
  // const INPUT_FILE: &str = "input.txt";

  let count = count_keypad_strokes(0, &[Keypad::Number(0), Keypad::Number(1)]);
  println!("{count}");

  Ok(())
}
