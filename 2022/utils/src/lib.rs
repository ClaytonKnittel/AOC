use clap::Parser;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[macro_use]
extern crate lazy_static;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
  /// Which part of the problem to run. Default is 1.
  #[arg(short, long, default_value_t = 1)]
  part: u32,
}

pub fn is_p1() -> bool {
  lazy_static! {
    static ref ARGS: Args = Args::parse();
  }

  ARGS.part == 1
}

pub fn get_input() -> Result<io::Lines<io::BufReader<File>>, std::io::Error> {
  let file = File::open(&Path::new("input.txt"))?;
  Ok(io::BufReader::new(file).lines())
}

pub fn all_input() -> Result<Vec<String>, std::io::Error> {
  Ok(
    get_input()?
      .into_iter()
      .collect::<Result<Vec<String>, std::io::Error>>()?,
  )
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let result = 2 + 2;
    assert_eq!(result, 4);
  }
}
