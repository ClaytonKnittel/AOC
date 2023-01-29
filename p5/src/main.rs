use regex::Regex;

#[macro_use]
extern crate lazy_static;

struct Crates {
  stacks: Vec<Vec<char>>,
}

impl Crates {
  pub fn new(width: u32) -> Self {
    Self {
      stacks: (0..width).map(|_1| vec![]).collect(),
    }
  }

  pub fn width(&self) -> u32 {
    self.stacks.len() as u32
  }

  pub fn add_stack_bottom(&mut self, stack_idx: usize, val: char) {
    self.stacks[stack_idx].insert(0, val);
  }
}

impl std::fmt::Display for Crates {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let h = self
      .stacks
      .iter()
      .fold(0usize, |h, stack| std::cmp::max(stack.len(), h));

    for i in (0..h).rev() {
      for j in 0..self.width() as usize {
        if j != 0 {
          write!(f, " ");
        }

        if self.stacks[j].len() <= i {
          write!(f, "   ");
        } else {
          write!(f, "[{}]", self.stacks[j][i]);
        }
      }
      write!(f, "\n");
    }
    for j in 0..self.width() as usize {
      if j != 0 {
        write!(f, " ");
      }
      write!(f, " {} ", j + 1);
    }

    Ok(())
  }
}

fn parse_crates(lines: &mut std::io::Lines<std::io::BufReader<std::fs::File>>) {
  lazy_static! {
    static ref CRATES_RE: Regex = Regex::new("^(\\[[A-Z]\\]| {3})( \\[[A-Z]\\]| {4})*$").unwrap();
    static ref IDX_RE: Regex = Regex::new("^ [1-9] (  [1-9] )*$").unwrap();
  }

  const item_w: u32 = 4u32;
  let mut crates_opt: Option<Crates> = None;

  loop {
    let line = lines.next().unwrap().unwrap();
    if !CRATES_RE.is_match(&line) {
      assert!(IDX_RE.is_match(&line));
      break;
    }

    match &crates_opt {
      Some(crates) => {
        assert_eq!(crates.width(), (line.len() as u32 + 1) / item_w);
      }
      None => {
        crates_opt = Some(Crates::new((line.len() as u32 + 1) / item_w));
      }
    }

    let crates = crates_opt.as_mut().unwrap();

    for (i, c) in line.chars().skip(1).step_by(item_w as usize).enumerate() {
      if c != ' ' {
        crates.add_stack_bottom(i, c);
      }
    }
  }

  assert_eq!(&lines.next().unwrap().unwrap(), "");

  println!("{}", crates_opt.unwrap());
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?;
  parse_crates(&mut contents);

  Ok(())
}
