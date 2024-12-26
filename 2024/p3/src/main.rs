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
        if !input.is_digit(10) {
          if digits == 0 {
            self.state = ParseState::M;
          } else if let ParseState::FirstNum { digits: _ } = self.state {
            self.state = ParseState::Comma;
            self = self.advance(input);
          } else {
            self.state = ParseState::CloseParen;
            self = self.advance(input);
          }
        } else {
          if digits == 3 {
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

  Ok(())
}
