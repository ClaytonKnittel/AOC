use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn get_input() -> Result<io::Lines<io::BufReader<File>>, std::io::Error> {
  let file = File::open(&Path::new("input.txt"))?;
  Ok(io::BufReader::new(file).lines())
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
