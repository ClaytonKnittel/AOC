use std::{error::Error, str::FromStr};

use once_cell::sync::Lazy;
use regex::Regex;
use util::error::{AocError, AocResult};

enum InputType {
  Literal,
  Combo,
}

enum RegisterInput {
  A,
  B,
  C,
}

enum OutputDestination {
  A,
  B,
  C,
  Console,
}

trait Instruction {
  type Dispatcher: InstructionDispatch;

  fn execute(computer: &mut Computer, opcode: u8, operand: u8) {
    Self::Dispatcher::dispatch(computer, opcode, operand)
  }
}

trait InstructionDispatch {
  fn dispatch(computer: &mut Computer, opcode: u8, operand: u8);
}

trait CpuInstruction: InstructionDispatch {
  const INPUT_TYPE: InputType;
  const REGISTER_INPUT: RegisterInput;
  const OUTPUT: OutputDestination;

  fn dispatch(computer: &mut Computer, opcode: u8, operand: u8) {
    todo!()
  }

  fn run(input: u64, register_input: u64) -> u64;
}

struct Adv {}

impl CpuInstruction for Adv {
  const INPUT_TYPE: InputType = InputType::Combo;
  const REGISTER_INPUT: RegisterInput = RegisterInput::A;
  const OUTPUT: OutputDestination = OutputDestination::A;

  fn run(input: u64, register_input: u64) -> u64 {}
}

struct Computer {
  a: u64,
  b: u64,
  c: u64,
  pc: usize,
}

impl Computer {
  fn execute(&mut self, opcode: u8, operand: u8) -> AocResult {
    Ok(())
  }
}

struct ProgramState {
  computer: Computer,
  program: Vec<u8>,
}

impl FromStr for ProgramState {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    static RE: Lazy<Regex> = Lazy::new(|| {
      Regex::new(r"^Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ([\d,]+)$")
        .unwrap()
    });

    let captures = RE
      .captures(s)
      .ok_or_else(|| AocError::Parse(format!("Failed to parse as ProgramState: {s}")))?;

    Ok(ProgramState {
      computer: Computer {
        a: captures[1].parse()?,
        b: captures[2].parse()?,
        c: captures[3].parse()?,
        pc: 0,
      },
      program: captures[4]
        .split(',')
        .map(|instruction_str| instruction_str.parse())
        .collect::<Result<_, _>>()?,
    })
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";

  Ok(())
}
