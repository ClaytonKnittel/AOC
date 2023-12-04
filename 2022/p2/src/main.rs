use regex::Regex;
use utils;

#[macro_use]
extern crate lazy_static;

#[derive(Copy, Clone)]
enum RPS {
  ROCK = 0,
  PAPER = 1,
  SCISSORS = 2,
}

#[derive(Copy, Clone)]
enum Res {
  LOSE = 0,
  TIE = 1,
  WIN = 2,
}

const RPS_MOVES: u32 = 3;

struct Outcome {
  p1_move: RPS,
  p2_move: RPS,
}

impl Outcome {
  fn new(p1_move: RPS, p2_move: RPS) -> Self {
    Self { p1_move, p2_move }
  }

  fn induce(outcome: Res, p2_move: RPS) -> Self {
    let out_ord = outcome as i32;
    let p2_ord = p2_move as i32;

    let p1_move =
      match (out_ord + p2_ord + (RPS_MOVES - (Res::TIE as u32)) as i32) % (RPS_MOVES as i32) {
        0 => RPS::ROCK,
        1 => RPS::PAPER,
        2 => RPS::SCISSORS,
        _ => panic!("Unreachable"),
      };

    Self { p1_move, p2_move }
  }

  fn score(&self) -> u64 {
    let s1 = self.p1_move as u32;
    let s2 = self.p2_move as u32;

    let choice_score = match self.p1_move {
      RPS::ROCK => 1,
      RPS::PAPER => 2,
      RPS::SCISSORS => 3,
    };

    let outcome_score = if s1 == s2 {
      3
    } else if (RPS_MOVES + s1 - s2) % RPS_MOVES == 1 {
      6
    } else {
      0
    };

    choice_score + outcome_score
  }
}

fn line_to_outcome(line_res: Result<String, std::io::Error>) -> Result<Outcome, std::io::Error> {
  lazy_static! {
    static ref OUTCOME_RE: Regex = Regex::new("^([ABC]) ([XYZ])$").unwrap();
  }

  let line = line_res?;
  let choices = OUTCOME_RE.captures(&line).ok_or(std::io::Error::new(
    std::io::ErrorKind::Other,
    format!("Invalid input line: {}", line),
  ))?;

  let their_move = match &choices[1] {
    "A" => RPS::ROCK,
    "B" => RPS::PAPER,
    "C" => RPS::SCISSORS,
    &_ => panic!("Unreachable"),
  };
  let outcome = match &choices[2] {
    "X" => Res::LOSE,
    "Y" => Res::TIE,
    "Z" => Res::WIN,
    &_ => panic!("Unreachable"),
  };

  Ok(Outcome::induce(outcome, their_move))
}

fn main() -> Result<(), std::io::Error> {
  let contents = utils::get_input()?;
  let outcomes = contents.fold(Ok(0), |total_score: Result<u64, std::io::Error>, line| {
    Ok(total_score? + line_to_outcome(line)?.score())
  })?;

  println!("Total score: {}", outcomes);

  Ok(())
}
