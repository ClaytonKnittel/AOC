use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

struct MaxSet<T: PartialOrd + Copy> {
  n: u32,
  top: Vec<T>,
}

impl<T: PartialOrd + Copy> MaxSet<T> {
  fn new(n: u32) -> Self {
    MaxSet {
      n: n,
      top: Vec::with_capacity(n as usize),
    }
  }

  fn push(&mut self, val: T) {
    if self.top.len() < self.n as usize {
      self.top.push(val)
    } else if self.top[0] < val {
      let mut i: usize = 1;
      while i < self.n as usize && self.top[i] < val {
        self.top[i - 1] = self.top[i];
        i += 1;
      }
      self.top[i - 1] = val;
    }
  }

  fn all<'a>(self: &'a Self) -> std::slice::Iter<T> {
    self.top.iter()
  }
}

fn get_input() -> Result<io::Lines<io::BufReader<File>>, std::io::Error> {
  let file = File::open(&Path::new("input.txt"))?;
  Ok(io::BufReader::new(file).lines())
}

fn parse(content: &mut io::Lines<io::BufReader<File>>) -> u64 {
  let mut cur_total: u64 = 0;
  let mut max_set = MaxSet::<u64>::new(3);

  for line in content {
    if let Ok(cnt_str) = line {
      if cnt_str.is_empty() {
        max_set.push(cur_total);
        cur_total = 0;
      } else if let Ok(cnt) = cnt_str.parse::<u64>() {
        cur_total += cnt;
      } else {
        panic!("Not a number: {}", cnt_str)
      }
    }
  }

  if cur_total != 0 {
    max_set.push(cur_total);
  }

  return max_set.all().sum();
}

fn main() -> Result<(), std::io::Error> {
  let mut content = get_input()?;

  println!("max: {}\n", parse(&mut content));

  Ok(())
}
