use clap::Parser;
use std::cmp;
use std::collections::HashMap;
use std::fmt;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
  /// How many rocks to drop. Default is 2022
  #[arg(short, default_value_t = 2022)]
  n: u64,

  /// if set, only print the timing in microseconds.
  #[arg(short, default_value_t = false)]
  t: bool,
}

#[derive(Clone, Copy)]
enum Direction {
  Left,
  Right,
}

struct WindPattern {
  winds: Vec<Direction>,
  idx: usize,
}

impl WindPattern {
  pub fn new(pattern: &str) -> Self {
    Self {
      winds: pattern
        .chars()
        .map(|c| match c {
          '<' => Direction::Left,
          '>' => Direction::Right,
          _ => panic!("Unknown direction!"),
        })
        .collect(),
      idx: 0,
    }
  }

  pub fn idx(&self) -> usize {
    self.idx
  }

  pub fn next(&mut self) -> Direction {
    let c = self.winds[self.idx];
    self.idx = if self.idx == self.winds.len() - 1 {
      0
    } else {
      self.idx + 1
    };
    c
  }
}

impl PartialEq for WindPattern {
  fn eq(&self, other: &Self) -> bool {
    self.idx == other.idx
  }
}

impl Eq for WindPattern {}

impl std::hash::Hash for WindPattern {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.idx.hash(state);
  }
}

impl Clone for WindPattern {
  fn clone(&self) -> Self {
    Self {
      winds: vec![],
      idx: self.idx,
    }
  }
}

#[derive(Clone)]
enum RockType {
  Hline,
  Plus,
  Backl,
  Vline,
  Boxx,
}

impl RockType {
  pub fn next(&self) -> Self {
    match self {
      RockType::Hline => RockType::Plus,
      RockType::Plus => RockType::Backl,
      RockType::Backl => RockType::Vline,
      RockType::Vline => RockType::Boxx,
      RockType::Boxx => RockType::Hline,
    }
  }
}

const ROCK_MAX_HEIGHT: u64 = 4;

#[derive(Clone)]
struct Rock {
  piece_mask: u32,
  rock_type: RockType,
  height: u64,
}

impl Rock {
  pub fn new(rock_type: RockType, height: u64) -> Self {
    const ROCKS: (u32, u32, u32, u32, u32) = (
      // ####
      0b00000000_00000000_00000000_00111100,
      // .#.
      // ###
      // .#.
      0b00000000_00001000_00011100_00001000,
      // ..#
      // ..#
      // ###
      0b00000000_00010000_00010000_00011100,
      // #
      // #
      // #
      // #
      0b00000100_00000100_00000100_00000100,
      // ##
      // ##
      0b00000000_00000000_00001100_00001100,
    );

    Self {
      piece_mask: match &rock_type {
        RockType::Hline => ROCKS.0,
        RockType::Plus => ROCKS.1,
        RockType::Backl => ROCKS.2,
        RockType::Vline => ROCKS.3,
        RockType::Boxx => ROCKS.4,
      },
      rock_type,
      height,
    }
  }

  pub fn mask(&self) -> u32 {
    self.piece_mask
  }

  pub fn height(&self) -> u64 {
    self.height
  }

  pub fn rock_type(&self) -> &RockType {
    &self.rock_type
  }

  pub fn top(&self) -> u64 {
    self.height()
      + match &self.rock_type {
        RockType::Hline => 1,
        RockType::Plus => 3,
        RockType::Backl => 3,
        RockType::Vline => 4,
        RockType::Boxx => 2,
      }
  }

  fn expected_bitcnt(&self) -> u32 {
    match &self.rock_type {
      RockType::Hline => 4,
      RockType::Plus => 5,
      RockType::Backl => 5,
      RockType::Vline => 4,
      RockType::Boxx => 4,
    }
  }

  pub fn push(&mut self, dir: Direction, window: &ChamberWindow) -> bool {
    let piece_mask = match dir {
      Direction::Left => self.piece_mask >> 1,
      Direction::Right => self.piece_mask << 1,
    } & 0x7f7f7f7f;

    if piece_mask.count_ones() != self.expected_bitcnt() || window.collides_mask(piece_mask) {
      return false;
    } else {
      self.piece_mask = piece_mask;
      return true;
    }
  }

  pub fn drop(&mut self) {
    self.height -= 1;
  }

  pub fn elevate_height(&mut self, height_gain: u64) {
    self.height += height_gain;
  }
}

impl PartialEq for Rock {
  fn eq(&self, other: &Self) -> bool {
    // Ignore height intentionally.
    self.piece_mask == other.piece_mask
  }
}

impl Eq for Rock {}

impl std::hash::Hash for Rock {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.piece_mask.hash(state);
  }
}

#[derive(Clone)]
struct ChamberWindow {
  window: u32,
}

impl ChamberWindow {
  pub fn new(rows: &[u8]) -> Self {
    // Assumes rows has at least 4 elements
    Self {
      window: (rows[0] as u32)
        | ((rows[1] as u32) << 8)
        | ((rows[2] as u32) << 16)
        | ((rows[3] as u32) << 24),
    }
  }

  pub fn write_back(&self, rows: &mut [u8]) {
    rows[0] = self.window as u8;
    rows[1] = (self.window >> 8) as u8;
    rows[2] = (self.window >> 16) as u8;
    rows[3] = (self.window >> 24) as u8;
  }

  pub fn slide(&mut self, next_row: u8) {
    self.window = (self.window << 8) | (next_row as u32);
  }

  pub fn collides(&self, rock: &Rock) -> bool {
    (self.window & rock.mask()) != 0
  }

  pub fn collides_mask(&self, rock_mask: u32) -> bool {
    (self.window & rock_mask) != 0
  }

  pub fn merge(&mut self, rock: &Rock) {
    self.window |= rock.mask();
  }

  pub fn has_full_row(&self) -> Option<u64> {
    (0..4)
      .rev()
      .find(|idx| ((self.window >> (8 * idx)) & 0x7f) == 0x7f)
  }
}

struct Chamber {
  rows: Vec<u8>,
  falling_rock: Rock,
  window: ChamberWindow,
  winds: WindPattern,
  bottom_height: u64,
  /// Map from Chamber instance to the number of rocks that had been dropped at
  /// this point.
  cache: HashMap<Chamber, u64>,
}

impl Chamber {
  pub fn new(winds: WindPattern) -> Self {
    let mut chamber = Self {
      rows: vec![0u8, 0u8, 0u8, 0u8],
      falling_rock: Rock::new(RockType::Hline, 0),
      window: ChamberWindow::new(&vec![0u8, 0u8, 0u8, 0u8]),
      winds,
      bottom_height: 0,
      cache: HashMap::new(),
    };

    chamber.first_4_drops();
    return chamber;
  }

  fn height(&self) -> u64 {
    match self.rows.iter().rev().position(|&row| row != 0) {
      Some(idx) => (self.rows.len() - idx) as u64 + self.bottom_height,
      None => self.bottom_height,
    }
  }

  fn elevate_height(&mut self, height_gain: u64) {
    self.bottom_height += height_gain;
    self.falling_rock.elevate_height(height_gain);
  }

  fn push(&mut self) -> bool {
    self.falling_rock.push(self.winds.next(), &self.window)
  }

  fn drop(&mut self) -> bool {
    assert!(self.falling_rock.height() >= self.bottom_height);
    if self.falling_rock.height() == self.bottom_height {
      return false;
    }

    let mut next_window = self.window.clone();
    next_window.slide(self.rows[(self.falling_rock.height() - self.bottom_height) as usize - 1]);

    if next_window.collides(&self.falling_rock) {
      return false;
    } else {
      self.falling_rock.drop();
      self.window = next_window;
      return true;
    }
  }

  fn first_4_drops(&mut self) {
    for _ in 0..4 {
      self.push();
    }
  }

  fn lock_falling_rock(&mut self) {
    let window = &mut self.window;
    let rock_height = self.falling_rock.height() as usize;

    window.merge(&self.falling_rock);
    window.write_back(
      &mut self.rows[rock_height - self.bottom_height as usize
        ..rock_height + ROCK_MAX_HEIGHT as usize - self.bottom_height as usize],
    );

    match window.has_full_row() {
      Some(row) => {
        let new_bottom = rock_height + row as usize + 1;
        self.rows.drain(0..new_bottom - self.bottom_height as usize);
        self.bottom_height = new_bottom as u64;
      }
      None => {}
    }
  }

  fn next_piece(&mut self) {
    self.rows.resize(
      cmp::max(
        (self.falling_rock.top() + ROCK_MAX_HEIGHT - self.bottom_height) as usize,
        self.rows.len(),
      ),
      0,
    );

    let h = self.rows.len() as u64 + self.bottom_height - ROCK_MAX_HEIGHT;
    let next_rock = Rock::new(self.falling_rock.rock_type().next(), h);

    self.window = ChamberWindow::new(&self.rows[(h - self.bottom_height) as usize..]);
    self.falling_rock = next_rock;

    self.first_4_drops();
  }

  pub fn do_rock_fall(&mut self) {
    while self.drop() {
      self.push();
    }

    self.lock_falling_rock();
    self.next_piece();
  }

  pub fn drop_until(&mut self, n_rocks: u64) {
    let mut rocks = 0u64;

    while rocks < n_rocks {
      let old_h = self.rows.len();

      self.do_rock_fall();
      rocks += 1;

      if self.rows.len() < old_h {
        match self.cache.get_key_value(&self) {
          Some((prev, prev_rocks)) => {
            let rocks_per_repeat = rocks - prev_rocks;
            let height_gain_per_repeat = self.height() - prev.height();
            let n_repeats_fit = (n_rocks - rocks) / rocks_per_repeat;

            let total_new_rocks = rocks_per_repeat * n_repeats_fit;
            let total_height_gain = height_gain_per_repeat * n_repeats_fit;

            rocks += total_new_rocks;
            self.elevate_height(total_height_gain);
          }
          None => {
            self.cache.insert(self.clone(), rocks);
          }
        }
      }
    }
  }
}

impl PartialEq for Chamber {
  fn eq(&self, other: &Self) -> bool {
    self.rows == other.rows && self.falling_rock == other.falling_rock && self.winds == other.winds
  }
}

impl Eq for Chamber {}

impl std::hash::Hash for Chamber {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.rows.hash(state);
    self.falling_rock.hash(state);
    self.winds.hash(state);
  }
}

impl Clone for Chamber {
  fn clone(&self) -> Self {
    Self {
      rows: self.rows.clone(),
      falling_rock: self.falling_rock.clone(),
      window: self.window.clone(),
      winds: self.winds.clone(),
      cache: HashMap::new(),
      ..*self
    }
  }
}

impl fmt::Display for Chamber {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let r_w = (0..)
      .take_while(|i| {
        10u64.pow(*i)
          <= std::cmp::max(
            self.rows.len() as u64 + self.bottom_height,
            self.winds.idx() as u64,
          )
      })
      .count();

    let mut rows: Vec<String> = self
      .rows
      .iter()
      .enumerate()
      .map(|(i, row)| {
        let row_str: String = (0..7)
          .map(|idx| if ((row >> idx) & 1) == 0 { '.' } else { '#' })
          .collect();
        format!("{:r_w$} ", i as u64 + self.bottom_height, r_w = r_w)
          + &String::from("|")
          + &row_str
          + &String::from("|")
      })
      .collect();

    for idx in 0..28u64 {
      let r = idx / 7;
      let i = idx % 7;

      if ((self.falling_rock.mask() >> (r * 8 + i)) & 1) == 1 {
        assert_eq!(
          rows[(self.falling_rock.height() + r - self.bottom_height) as usize]
            .chars()
            .nth(i as usize + 2 + r_w)
            .unwrap(),
          '.'
        );
        rows[(self.falling_rock.height() + r - self.bottom_height) as usize]
          .replace_range(i as usize + 2 + r_w..i as usize + 3 + r_w, "@");
      }
    }

    let disp = rows.iter().fold(
      format!("{:r_w$} +-------+", self.winds.idx(), r_w = r_w),
      |disp, row_str| row_str.to_owned() + &String::from("\n") + &disp,
    );
    write!(f, "{}", disp)
  }
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?
    .into_iter()
    .collect::<Result<Vec<String>, std::io::Error>>()?;
  let wind = contents.remove(0);
  assert_eq!(contents.len(), 0);
  let args = Args::parse();
  let n = args.n;

  let start = std::time::Instant::now();

  let mut chamber = Chamber::new(WindPattern::new(&wind));

  chamber.drop_until(n);

  let h = chamber.height();
  let end = std::time::Instant::now();

  if args.t {
    println!("{}", (end - start).as_micros());
  } else {
    println!("{} in {:?}", h, end - start);
  }

  Ok(())
}
