use regex::Regex;

#[macro_use]
extern crate lazy_static;

struct Move {
  pub from_idx: usize,
  pub to_idx: usize,
  pub cnt: usize,
}

impl Move {
  pub fn new(from_idx: usize, to_idx: usize, cnt: usize) -> Self {
    Self {
      from_idx,
      to_idx,
      cnt,
    }
  }
}

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

  pub fn do_move_9000(&mut self, m: &Move) {
    for _ in 0..m.cnt {
      let c = self.stacks[m.from_idx].pop().unwrap();
      self.stacks[m.to_idx].push(c);
    }
  }

  pub fn do_move_9001(&mut self, m: &Move) {
    let stack = &mut self.stacks[m.from_idx];
    let cs = stack.drain(stack.len() - m.cnt..).collect::<Vec<_>>();
    self.stacks[m.to_idx].extend(cs);
  }

  pub fn tops(&self) -> String {
    self.stacks.iter().fold(String::from(""), |s, stack| {
      s + &stack[stack.len() - 1].to_string()
    })
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
          write!(f, " ")?;
        }

        if self.stacks[j].len() <= i {
          write!(f, "   ")?;
        } else {
          write!(f, "[{}]", self.stacks[j][i])?;
        }
      }
      write!(f, "\n")?;
    }
    for j in 0..self.width() as usize {
      if j != 0 {
        write!(f, " ")?;
      }
      write!(f, " {} ", j + 1)?;
    }

    Ok(())
  }
}

fn parse_crates(lines: &mut std::io::Lines<std::io::BufReader<std::fs::File>>) -> Crates {
  lazy_static! {
    static ref CRATES_RE: Regex = Regex::new("^(\\[[A-Z]\\]| {3})( \\[[A-Z]\\]| {4})*$").unwrap();
    static ref IDX_RE: Regex = Regex::new("^ [1-9] (  [1-9] )*$").unwrap();
  }

  const ITEM_W: u32 = 4u32;
  let mut crates_opt: Option<Crates> = None;

  loop {
    let line = lines.next().unwrap().unwrap();
    if !CRATES_RE.is_match(&line) {
      assert!(IDX_RE.is_match(&line));
      break;
    }

    match &crates_opt {
      Some(crates) => {
        assert_eq!(crates.width(), (line.len() as u32 + 1) / ITEM_W);
      }
      None => {
        crates_opt = Some(Crates::new((line.len() as u32 + 1) / ITEM_W));
      }
    }

    let crates = crates_opt.as_mut().unwrap();

    for (i, c) in line.chars().skip(1).step_by(ITEM_W as usize).enumerate() {
      if c != ' ' {
        crates.add_stack_bottom(i, c);
      }
    }
  }

  assert_eq!(&lines.next().unwrap().unwrap(), "");

  crates_opt.unwrap()
}

fn parse_move(line: &str) -> Move {
  lazy_static! {
    static ref MOVE_RE: Regex = Regex::new("^move ([0-9]+) from ([0-9]+) to ([0-9]+)$").unwrap();
  }

  let move_info = MOVE_RE.captures(line).unwrap();

  Move::new(
    move_info[2].parse::<usize>().unwrap() - 1,
    move_info[3].parse::<usize>().unwrap() - 1,
    move_info[1].parse::<usize>().unwrap(),
  )
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?;
  let mut crates = parse_crates(&mut contents);

  if utils::is_p1() {
    contents.for_each(|line| {
      let m = parse_move(&line.unwrap());
      crates.do_move_9000(&m);
    });
  } else {
    contents.for_each(|line| {
      let m = parse_move(&line.unwrap());
      crates.do_move_9001(&m);
    });
  }

  println!("{}", crates);
  println!("{}", crates.tops());

  Ok(())
}
