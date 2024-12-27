use std::{
  fs::File,
  io::{BufReader, Read},
};

use util::error::AocResult;

#[derive(Clone, Copy)]
enum ParseState {
  M,
  U,
  L,
  OpenParen,
  FirstNum { digits: u32 },
  Comma,
  SecondNum { digits: u32 },
  CloseParen,
}

struct ParseMachine {
  state: ParseState,
  first_num: u32,
  second_num: u32,
  products: u32,
}

impl ParseMachine {
  fn new() -> Self {
    Self {
      state: ParseState::M,
      first_num: 0,
      second_num: 0,
      products: 0,
    }
  }

  fn advance(mut self, input: char) -> Self {
    match self.state {
      s @ (ParseState::M
      | ParseState::U
      | ParseState::L
      | ParseState::OpenParen
      | ParseState::Comma
      | ParseState::CloseParen) => {
        let expected_char = match s {
          ParseState::M => 'm',
          ParseState::U => 'u',
          ParseState::L => 'l',
          ParseState::OpenParen => '(',
          ParseState::Comma => ',',
          ParseState::CloseParen => ')',
          _ => unreachable!(),
        };
        if input != expected_char {
          self.state = ParseState::M;
        } else {
          self.state = match s {
            ParseState::M => ParseState::U,
            ParseState::U => ParseState::L,
            ParseState::L => ParseState::OpenParen,
            ParseState::OpenParen => {
              self.first_num = 0;
              ParseState::FirstNum { digits: 0 }
            }
            ParseState::Comma => {
              self.second_num = 0;
              ParseState::SecondNum { digits: 0 }
            }
            ParseState::CloseParen => {
              self.products += self.first_num * self.second_num;
              ParseState::M
            }
            _ => unreachable!(),
          };
        }
      }
      ParseState::FirstNum { digits } | ParseState::SecondNum { digits } => {
        if !input.is_ascii_digit() {
          if digits == 0 {
            self.state = ParseState::M;
          } else if let ParseState::FirstNum { digits: _ } = self.state {
            self.state = ParseState::Comma;
            self = self.advance(input);
          } else {
            self.state = ParseState::CloseParen;
            self = self.advance(input);
          }
        } else if digits == 3 {
          self.state = ParseState::M;
        } else {
          let num = match &mut self.state {
            ParseState::FirstNum { digits } => {
              *digits += 1;
              &mut self.first_num
            }
            ParseState::SecondNum { digits } => {
              *digits += 1;
              &mut self.second_num
            }
            _ => unreachable!(),
          };
          *num = (10 * *num) + input.to_digit(10).unwrap();
        }
      }
    }

    self
  }
}

#[derive(Clone, Copy)]
enum ParseState2 {
  M,
  U,
  L,
  OpenParen,
  FirstNum { digits: u32 },
  Comma,
  SecondNum { digits: u32 },
  CloseParen,
  O,
  CloseParen2,
  NOrParen,
  Apostrophe,
  T,
  OpenParen3,
  CloseParen3,
}

struct ParseMachine2 {
  state: ParseState2,
  should: bool,
  first_num: u32,
  second_num: u32,
  products: u32,
}

impl ParseMachine2 {
  fn new() -> Self {
    Self {
      state: ParseState2::M,
      should: true,
      first_num: 0,
      second_num: 0,
      products: 0,
    }
  }

  fn advance(mut self, input: char) -> Self {
    match self.state {
      ParseState2::M => {
        self.state = match input {
          'm' => ParseState2::U,
          'd' => ParseState2::O,
          _ => ParseState2::M,
        };
      }
      s @ (ParseState2::U
      | ParseState2::L
      | ParseState2::OpenParen
      | ParseState2::Comma
      | ParseState2::CloseParen
      | ParseState2::O
      | ParseState2::CloseParen2
      | ParseState2::Apostrophe
      | ParseState2::T
      | ParseState2::OpenParen3
      | ParseState2::CloseParen3) => {
        let expected_char = match s {
          ParseState2::M => 'm',
          ParseState2::U => 'u',
          ParseState2::L => 'l',
          ParseState2::OpenParen => '(',
          ParseState2::Comma => ',',
          ParseState2::CloseParen => ')',
          ParseState2::O => 'o',
          ParseState2::CloseParen2 => ')',
          ParseState2::Apostrophe => '\'',
          ParseState2::T => 't',
          ParseState2::OpenParen3 => '(',
          ParseState2::CloseParen3 => ')',
          _ => unreachable!(),
        };
        if input != expected_char {
          self.state = ParseState2::M;
        } else {
          self.state = match s {
            ParseState2::M => ParseState2::U,
            ParseState2::U => ParseState2::L,
            ParseState2::L => ParseState2::OpenParen,
            ParseState2::OpenParen => {
              self.first_num = 0;
              ParseState2::FirstNum { digits: 0 }
            }
            ParseState2::Comma => {
              self.second_num = 0;
              ParseState2::SecondNum { digits: 0 }
            }
            ParseState2::CloseParen => {
              if self.should {
                self.products += self.first_num * self.second_num;
              }
              ParseState2::M
            }
            ParseState2::O => ParseState2::NOrParen,
            ParseState2::CloseParen2 => {
              self.should = true;
              ParseState2::M
            }
            ParseState2::Apostrophe => ParseState2::T,
            ParseState2::T => ParseState2::OpenParen3,
            ParseState2::OpenParen3 => ParseState2::CloseParen3,
            ParseState2::CloseParen3 => {
              self.should = false;
              ParseState2::M
            }
            _ => unreachable!(),
          };
        }
      }
      ParseState2::NOrParen => {
        self.state = match input {
          'n' => ParseState2::Apostrophe,
          '(' => ParseState2::CloseParen2,
          _ => ParseState2::M,
        };
      }
      ParseState2::FirstNum { digits } | ParseState2::SecondNum { digits } => {
        if !input.is_ascii_digit() {
          if digits == 0 {
            self.state = ParseState2::M;
          } else if let ParseState2::FirstNum { digits: _ } = self.state {
            self.state = ParseState2::Comma;
            self = self.advance(input);
          } else {
            self.state = ParseState2::CloseParen;
            self = self.advance(input);
          }
        } else if digits == 3 {
          self.state = ParseState2::M;
        } else {
          let num = match &mut self.state {
            ParseState2::FirstNum { digits } => {
              *digits += 1;
              &mut self.first_num
            }
            ParseState2::SecondNum { digits } => {
              *digits += 1;
              &mut self.second_num
            }
            _ => unreachable!(),
          };
          *num = (10 * *num) + input.to_digit(10).unwrap();
        }
      }
    }

    self
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let input = BufReader::new(File::open(INPUT_FILE)?);

  let machine = input
    .bytes()
    .try_fold(ParseMachine::new(), |machine, b| -> AocResult<_> {
      Ok(machine.advance(b? as char))
    })?;

  println!("Total products: {}", machine.products);

  let input = BufReader::new(File::open(INPUT_FILE)?);
  let machine2 = input
    .bytes()
    .try_fold(ParseMachine2::new(), |machine, b| -> AocResult<_> {
      Ok(machine.advance(b? as char))
    })?;

  println!("Total products with do's: {}", machine2.products);

  Ok(())
}
