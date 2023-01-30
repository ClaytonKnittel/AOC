#[derive(Clone, Copy)]
enum Direction {
  left,
  right,
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
          '<' => Direction::left,
          '>' => Direction::right,
          _ => panic!("Unknown direction!"),
        })
        .collect(),
      idx: 0,
    }
  }

  pub fn next(&mut self) -> Direction {
    let c = self.winds[self.idx];
    self.idx = if self.idx == self.winds.len() {
      0
    } else {
      self.idx + 1
    };
    c
  }
}

enum RockType {
  hline,
  plus,
  backl,
  vline,
  boxx,
}

impl RockType {
  pub fn next(&self) -> Self {
    match self {
      hline => RockType::plus,
      plus => RockType::backl,
      backl => RockType::vline,
      vline => RockType::boxx,
      boxx => RockType::hline,
    }
  }
}

struct Rock {
  piece_mask: u32,
  height: u32,
  rock_type: RockType,
}

impl Rock {
  pub fn new(rock_type: RockType, height: u32) -> Self {
    const rocks: (u32, u32, u32, u32, u32) = (
      // ####
      0b00111100_00000000_00000000_00000000u32,
      // .#.
      // ###
      // .#.
      0b00010000_00111000_00010000_00000000u32,
      // ..#
      // ..#
      // ###
      0b00111000_00001000_00001000_00000000u32,
      // #
      // #
      // #
      // #
      0b00100000_00100000_00100000_00100000u32,
      // ##
      // ##
      0b00110000_00110000_00000000_00000000u32,
    );

    Self {
      piece_mask: match &rock_type {
        hline => rocks.0,
        plus => rocks.1,
        backl => rocks.2,
        vline => rocks.3,
        boxx => rocks.4,
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
        hline => 1,
        plus => 3,
        backl => 3,
        vline => 4,
        boxx => 2,
      }
  }

  pub fn push(&mut self, dir: Direction) {
    match dir {
      left => self.piece_mask <<= 1,
      right => self.piece_mask >>= 1,
    };
  }

  pub fn drop(&mut self) {
    self.height -= 1;
  }
}

struct ChamberWindow {
  window: u32,
}

impl ChamberWindow {
  pub fn new(rows: &Vec<u8>) -> Self {
    // Assumes rows has at least 4 elements
    Self {
      window: (rows[rows.len() - 4] as u32)
        | ((rows[rows.len() - 3] as u32) << 8)
        | ((rows[rows.len() - 2] as u32) << 16)
        | ((rows[rows.len() - 1] as u32) << 24),
    }
  }

  pub fn slide(&mut self, next_row: u8) {
    self.window = (self.window << 8) | (next_row as u32);
  }

  pub fn collides(&self, rock: Rock) -> bool {
    (self.window & rock.mask()) != 0
  }
}

struct Chamber {
  rows: Vec<u8>,
  next_rock_type: RockType,
  falling_rock: Rock,
  window: ChamberWindow,
  winds: WindPattern,
}

impl Chamber {
  pub fn new(winds: WindPattern) -> Self {
    Self {
      rows: vec![0u8, 0u8, 0u8, 0u8],
      next_rock_type: RockType::hline,
      falling_rock: Rock::new(RockType::boxx, u32::MAX - 1),
      window: ChamberWindow::new(&vec![0u8, 0u8, 0u8, 0u8]),
      winds,
    }
  }

  pub fn push(&mut self, dir: Direction) -> bool {
    false
  }

  pub fn drop(&mut self) -> bool {
    false
  }

  pub fn next_piece(&mut self) {
    let next_rock = Rock::new(
      self.falling_rock.rock_type().next(),
      self.falling_rock.top(),
    );
  }
}

fn main() -> Result<(), std::io::Error> {
  let mut contents = utils::get_input()?
    .into_iter()
    .collect::<Result<Vec<String>, std::io::Error>>()?;
  let wind = contents.remove(0);
  assert_eq!(contents.len(), 0);

  Ok(())
}
