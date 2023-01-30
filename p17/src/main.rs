use std::cmp;
use std::fmt;

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

const ROCK_MAX_HEIGHT: u32 = 4;

struct Rock {
  piece_mask: u32,
  height: u32,
  rock_type: RockType,
}

impl Rock {
  pub fn new(rock_type: RockType, height: u32) -> Self {
    const ROCKS: (u32, u32, u32, u32, u32) = (
      // ####
      0b00000000_00000000_00000000_00111100u32,
      // .#.
      // ###
      // .#.
      0b00000000_00001000_00011100_00001000u32,
      // ..#
      // ..#
      // ###
      0b00000000_00010000_00010000_00011100u32,
      // #
      // #
      // #
      // #
      0b00000100_00000100_00000100_00000100u32,
      // ##
      // ##
      0b00000000_00000000_00001100_00001100u32,
    );

    Self {
      piece_mask: match &rock_type {
        RockType::Hline => ROCKS.0,
        RockType::Plus => ROCKS.1,
        RockType::Backl => ROCKS.2,
        RockType::Vline => ROCKS.3,
        RockType::Boxx => ROCKS.4,
      },
      height,
      rock_type,
    }
  }

  pub fn mask(&self) -> u32 {
    self.piece_mask
  }

  pub fn height(&self) -> u32 {
    self.height
  }

  pub fn rock_type(&self) -> &RockType {
    &self.rock_type
  }

  pub fn top(&self) -> u32 {
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
}

struct Chamber {
  rows: Vec<u8>,
  falling_rock: Rock,
  window: ChamberWindow,
  winds: WindPattern,
}

impl Chamber {
  pub fn new(winds: WindPattern) -> Self {
    let mut chamber = Self {
      rows: vec![0u8, 0u8, 0u8, 0u8],
      falling_rock: Rock::new(RockType::Hline, 0),
      window: ChamberWindow::new(&vec![0u8, 0u8, 0u8, 0u8]),
      winds,
    };

    chamber.first_4_drops();
    return chamber;
  }

  fn height(&self) -> u32 {
    match self.rows.iter().rev().position(|&row| row != 0) {
      Some(idx) => (self.rows.len() - idx) as u32,
      None => 0,
    }
  }

  fn push(&mut self) -> bool {
    self.falling_rock.push(self.winds.next(), &self.window)
  }

  fn drop(&mut self) -> bool {
    if self.falling_rock.height() == 0 {
      return false;
    }

    let mut next_window = self.window.clone();
    next_window.slide(self.rows[self.falling_rock.height() as usize - 1]);

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
    window.write_back(&mut self.rows[rock_height..rock_height + ROCK_MAX_HEIGHT as usize]);
  }

  fn next_piece(&mut self) {
    self.rows.resize(
      cmp::max(
        (self.falling_rock.top() + ROCK_MAX_HEIGHT) as usize,
        self.rows.len(),
      ),
      0,
    );

    let h = self.rows.len() as u32 - ROCK_MAX_HEIGHT;
    let next_rock = Rock::new(self.falling_rock.rock_type().next(), h);

    self.window = ChamberWindow::new(&self.rows[h as usize..]);
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
}

impl fmt::Display for Chamber {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let mut rows: Vec<String> = self
      .rows
      .iter()
      .map(|row| {
        let row_str: String = (0..7)
          .map(|idx| if ((row >> idx) & 1) == 0 { '.' } else { '#' })
          .collect();
        String::from("|") + &row_str + &String::from("|")
      })
      .collect();

    for idx in 0..28u32 {
      let r = idx / 7;
      let i = idx % 7;

      if ((self.falling_rock.mask() >> (r * 8 + i)) & 1) == 1 {
        assert_eq!(
          rows[(self.falling_rock.height() + r) as usize]
            .chars()
            .nth(i as usize + 1)
            .unwrap(),
          '.'
        );
        rows[(self.falling_rock.height() + r) as usize]
          .replace_range(i as usize + 1..i as usize + 2, "@");
      }
    }

    let disp = rows
      .iter()
      .fold(String::from("+-------+"), |disp, row_str| {
        row_str.to_owned() + &String::from("\n") + &disp
      });
    write!(f, "{}", disp)
  }
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?
    .into_iter()
    .collect::<Result<Vec<String>, std::io::Error>>()?;
  let wind = contents.remove(0);
  assert_eq!(contents.len(), 0);

  let mut chamber = Chamber::new(WindPattern::new(&wind));

  for _ in 0..2022 {
    chamber.do_rock_fall();
  }

  println!("{}", chamber.height());

  Ok(())
}
