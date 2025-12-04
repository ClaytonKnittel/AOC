use std::{process::ExitCode, str::FromStr};

use clap::{arg, command, Parser};
use strum::{EnumIter, IntoEnumIterator};
use util::error::{AocError, AocResult};

use crate::{
  p1::P1,
  p2::P2,
  p3::P3,
  p4::P4,
  solution::{Part, Solution},
};

mod p1;
mod p2;
mod p3;
mod p4;
mod solution;

#[derive(EnumIter, Debug, Clone, Copy)]
enum Problem {
  P1,
  P2,
  P3,
  P4,
}

impl Problem {
  fn run_part(&self, part: Part) -> AocResult {
    match self {
      Self::P1 => println!("Problem 1 {part}: {}", P1::solve("inputs/p1.txt", part)?),
      Self::P2 => println!("Problem 2 {part}: {}", P2::solve("inputs/p2.txt", part)?),
      Self::P3 => println!("Problem 3 {part}: {}", P3::solve("inputs/p3.txt", part)?),
      Self::P4 => println!("Problem 4 {part}: {}", P4::solve("inputs/p4.txt", part)?),
    }

    Ok(())
  }

  fn run(&self, part: Option<Part>) -> AocResult {
    match part {
      Some(part) => self.run_part(part),
      None => {
        self.run_part(Part::P1)?;
        self.run_part(Part::P2)
      }
    }
  }
}

impl FromStr for Problem {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "1" => Ok(Self::P1),
      "2" => Ok(Self::P2),
      "3" => Ok(Self::P3),
      "4" => Ok(Self::P4),
      _ => Err(AocError::Parse(format!(
        "Failed to parse \"{s}\" as a problem. Expected 1-25, or \"all\""
      ))),
    }
  }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
  /// Which problem to run. Can be a number between 1-25. If not specified, all problems will be run.
  #[arg(short, long)]
  problem: Option<Problem>,

  /// Which part of the problem to run. Can be part 1 ("1") or part 2 ("2").
  #[arg(long)]
  part: Option<Part>,
}

fn run() -> AocResult {
  let args = Args::try_parse()?;

  match args.problem {
    Some(problem) => problem.run(args.part)?,
    None => {
      for problem in Problem::iter() {
        problem.run(args.part)?;
      }
    }
  }

  Ok(())
}

fn main() -> ExitCode {
  let result = run();
  if let Err(err) = result {
    println!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
