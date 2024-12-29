use std::{
  error::Error,
  fs::File,
  io::{BufReader, Read},
  iter::successors,
  marker::PhantomData,
  str::FromStr,
};

use once_cell::sync::Lazy;
use regex::Regex;
use util::error::{AocError, AocResult};

enum InputType {
  Literal,
  Combo,
}

enum Register {
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
  fn execute(computer: &mut Computer, operand: u8) -> Option<u8>;
}

trait NumericInstruction {
  const OUTPUT: OutputDestination;

  fn execute(computer: &mut Computer, operand: u8) -> u64;
}

impl<T: NumericInstruction> Instruction for T {
  fn execute(computer: &mut Computer, operand: u8) -> Option<u8> {
    let output = Self::execute(computer, operand);
    computer.pc += 2;

    match Self::OUTPUT {
      OutputDestination::A => {
        computer.a = output;
        None
      }
      OutputDestination::B => {
        computer.b = output;
        None
      }
      OutputDestination::C => {
        computer.c = output;
        None
      }
      OutputDestination::Console => Some((output & 0x7) as u8),
    }
  }
}

trait CpuInstruction: NumericInstruction {
  const INPUT_TYPE: InputType;
  const REGISTER_INPUT: Register;
  const REGISTER_OUTPUT: Register;

  fn run(input: u64, register_input: u64) -> u64;
}

impl<T: CpuInstruction> NumericInstruction for T {
  const OUTPUT: OutputDestination = match Self::REGISTER_OUTPUT {
    Register::A => OutputDestination::A,
    Register::B => OutputDestination::B,
    Register::C => OutputDestination::C,
  };

  fn execute(computer: &mut Computer, operand: u8) -> u64 {
    let input = match Self::INPUT_TYPE {
      InputType::Literal => operand as u64,
      InputType::Combo => computer.combo(operand),
    };
    let register_input = match Self::REGISTER_INPUT {
      Register::A => computer.a,
      Register::B => computer.b,
      Register::C => computer.c,
    };

    T::run(input, register_input)
  }
}

trait Div {
  const OUTPUT_REGISTER: Register;
}
impl<T: Div> CpuInstruction for T {
  const INPUT_TYPE: InputType = InputType::Combo;
  const REGISTER_INPUT: Register = Register::A;
  const REGISTER_OUTPUT: Register = <Self as Div>::OUTPUT_REGISTER;

  fn run(input: u64, register_input: u64) -> u64 {
    register_input >> input
  }
}

struct Adv {}
impl Div for Adv {
  const OUTPUT_REGISTER: Register = Register::A;
}
struct Bdv {}
impl Div for Bdv {
  const OUTPUT_REGISTER: Register = Register::B;
}
struct Cdv {}
impl Div for Cdv {
  const OUTPUT_REGISTER: Register = Register::C;
}

struct Bxl {}
impl CpuInstruction for Bxl {
  const INPUT_TYPE: InputType = InputType::Literal;
  const REGISTER_INPUT: Register = Register::B;
  const REGISTER_OUTPUT: Register = Register::B;

  fn run(input: u64, register_input: u64) -> u64 {
    register_input ^ input
  }
}

struct Bst {}
impl CpuInstruction for Bst {
  const INPUT_TYPE: InputType = InputType::Combo;
  const REGISTER_INPUT: Register = Register::B;
  const REGISTER_OUTPUT: Register = Register::B;

  fn run(input: u64, _: u64) -> u64 {
    input & 0x7
  }
}

struct Jnz {}
impl Instruction for Jnz {
  fn execute(computer: &mut Computer, operand: u8) -> Option<u8> {
    if computer.a == 0 {
      computer.pc += 2;
    } else {
      computer.pc = operand as usize;
    }
    None
  }
}

struct Bxc {}
impl NumericInstruction for Bxc {
  const OUTPUT: OutputDestination = OutputDestination::B;

  fn execute(computer: &mut Computer, _: u8) -> u64 {
    computer.b ^ computer.c
  }
}

struct Out {}
impl NumericInstruction for Out {
  const OUTPUT: OutputDestination = OutputDestination::Console;

  fn execute(computer: &mut Computer, operand: u8) -> u64 {
    computer.combo(operand)
  }
}

#[derive(Clone, Debug)]
struct Computer {
  a: u64,
  b: u64,
  c: u64,
  pc: usize,
}

impl Computer {
  fn combo(&self, operand: u8) -> u64 {
    match operand {
      0..=3 => operand as u64,
      4 => self.a,
      5 => self.b,
      6 => self.c,
      _ => unreachable!(),
    }
  }
}

struct ProgramState<I0, I1, I2, I3, I4, I5, I6, I7> {
  computer: Computer,
  program: Vec<u8>,
  #[allow(clippy::type_complexity)]
  _phony: PhantomData<(I0, I1, I2, I3, I4, I5, I6, I7)>,
}

impl<I0, I1, I2, I3, I4, I5, I6, I7> FromStr for ProgramState<I0, I1, I2, I3, I4, I5, I6, I7> {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    static RE: Lazy<Regex> = Lazy::new(|| {
      Regex::new(r"^Register A: (\d+)\nRegister B: (\d+)\nRegister C: (\d+)\n\nProgram: ([\d,]+)$")
        .unwrap()
    });

    let captures = RE
      .captures(s)
      .ok_or_else(|| AocError::Parse(format!("Failed to parse as ProgramState: {s}")))?;

    Ok(Self {
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
      _phony: PhantomData,
    })
  }
}

impl<I0, I1, I2, I3, I4, I5, I6, I7> ProgramState<I0, I1, I2, I3, I4, I5, I6, I7>
where
  I0: Instruction,
  I1: Instruction,
  I2: Instruction,
  I3: Instruction,
  I4: Instruction,
  I5: Instruction,
  I6: Instruction,
  I7: Instruction,
{
  fn run(&mut self) -> impl Iterator<Item = u8> + '_ {
    successors(Some(None), |_| {
      (self.computer.pc < self.program.len()).then(|| {
        let pc = self.computer.pc;
        match (self.program[pc], self.program[pc + 1]) {
          (0, operand) => I0::execute(&mut self.computer, operand),
          (1, operand) => I1::execute(&mut self.computer, operand),
          (2, operand) => I2::execute(&mut self.computer, operand),
          (3, operand) => I3::execute(&mut self.computer, operand),
          (4, operand) => I4::execute(&mut self.computer, operand),
          (5, operand) => I5::execute(&mut self.computer, operand),
          (6, operand) => I6::execute(&mut self.computer, operand),
          (7, operand) => I7::execute(&mut self.computer, operand),
          _ => unreachable!(),
        }
      })
    })
    .flatten()
  }

  fn is_quine(&self) -> bool {
    self.program.iter().copied().eq(self.clone().run())
  }

  fn find_quine_helper(
    computer: Computer,
    orig_program: &Vec<u8>,
    target_program: &[u8],
  ) -> Option<u64> {
    if target_program.is_empty() {
      return Some(computer.a);
    }

    let target_out = target_program[target_program.len() - 1];
    (0..8).find_map(|next_3| {
      let computer = Computer {
        a: (computer.a << 3) | next_3,
        ..computer
      };
      let mut program = Self {
        computer: computer.clone(),
        program: orig_program.clone(),
        _phony: PhantomData,
      };

      if program.run().next() == Some(target_out) {
        Self::find_quine_helper(
          computer.clone(),
          orig_program,
          &target_program[..(target_program.len() - 1)],
        )
      } else {
        None
      }
    })
  }

  fn find_quine_modifying_a(&self) -> u64 {
    let mut local_program = self.clone();
    // Remove last instruction (the jump).
    local_program.program.pop();
    local_program.program.pop();

    // Derive the instruction opcodes backwards
    Self::find_quine_helper(
      Computer {
        a: 0,
        ..self.computer
      },
      &self.program,
      &self.program,
    )
    .unwrap()
  }
}

impl<I0, I1, I2, I3, I4, I5, I6, I7> Clone for ProgramState<I0, I1, I2, I3, I4, I5, I6, I7> {
  fn clone(&self) -> Self {
    Self {
      computer: self.computer.clone(),
      program: self.program.clone(),
      _phony: PhantomData,
    }
  }
}

type DefaultProgram = ProgramState<Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv>;

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let mut program_str = String::new();
  BufReader::new(File::open(INPUT_FILE)?).read_to_string(&mut program_str)?;
  let program: DefaultProgram = program_str.parse()?;

  let output = program
    .clone()
    .run()
    .map(|output| output.to_string())
    .collect::<Vec<_>>()
    .join(",");
  println!("Output: {output}");

  let quine_a = program.find_quine_modifying_a();
  println!("Quine A: {quine_a}");

  Ok(())
}
