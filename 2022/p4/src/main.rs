use regex::Regex;

#[macro_use]
extern crate lazy_static;

struct Range {
  l: u32,
  u: u32,
}

impl Range {
  pub fn new(l: u32, u: u32) -> Self {
    Self { l, u }
  }

  pub fn contains(&self, other: &Self) -> bool {
    self.l <= other.l && self.u >= other.u
  }

  pub fn overlaps(&self, other: &Self) -> bool {
    self.u >= other.l && self.l <= other.u
  }
}

fn parse_ranges(line: &str) -> Result<(Range, Range), std::num::ParseIntError> {
  lazy_static! {
    static ref LINE_RE: Regex = Regex::new("^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$").unwrap();
  }

  let ranges = LINE_RE.captures(line).unwrap();

  Ok((
    Range::new(ranges[1].parse()?, ranges[2].parse()?),
    Range::new(ranges[3].parse()?, ranges[4].parse()?),
  ))
}

fn line_shadows(line: &str) -> bool {
  let (r1, r2) = parse_ranges(line).unwrap();
  r1.contains(&r2) || r2.contains(&r1)
}

fn line_overlaps(line: &str) -> bool {
  let (r1, r2) = parse_ranges(line).unwrap();
  r1.overlaps(&r2)
}

fn main() -> Result<(), std::io::Error> {
  let contents = utils::get_input()?;

  let num_shadows = if utils::is_p1() {
    contents.fold(0u32, |cnt, line| {
      if line_shadows(&line.unwrap()) {
        cnt + 1
      } else {
        cnt
      }
    })
  } else {
    contents.fold(0u32, |cnt, line| {
      if line_overlaps(&line.unwrap()) {
        cnt + 1
      } else {
        cnt
      }
    })
  };

  println!("Num shadowed pairs: {}", num_shadows);

  Ok(())
}
