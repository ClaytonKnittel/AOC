use std::{
  error::Error,
  fmt::{self, Display, Formatter},
  fs::read_to_string,
  ops::{BitAnd, BitOr, BitXor},
  str::FromStr,
};

use util::{
  error::{AocError, AocResult},
  iter_util::iter_to_slice,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Bool {
  val: bool,
}

impl BitAnd<Self> for Bool {
  type Output = Self;

  fn bitand(self, rhs: Self) -> Self::Output {
    Self {
      val: self.val && rhs.val,
    }
  }
}

impl BitOr<Self> for Bool {
  type Output = Self;

  fn bitor(self, rhs: Self) -> Self::Output {
    Self {
      val: self.val || rhs.val,
    }
  }
}

impl BitXor<Self> for Bool {
  type Output = Self;

  fn bitxor(self, rhs: Self) -> Self::Output {
    Self {
      val: self.val ^ rhs.val,
    }
  }
}

impl FromStr for Bool {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "0" => Ok(Self { val: false }),
      "1" => Ok(Self { val: true }),
      _ => Err(AocError::Parse(format!("Could not parse \"{s}\" as Bool")).into()),
    }
  }
}

impl Display for Bool {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", if self.val { '1' } else { '0' })
  }
}

enum Statement<T> {
  Declaration(Declaration<T>),
  Condition(Condition<T>),
}

impl<T: Display> Display for Statement<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Declaration(declaration) => {
        write!(f, "{declaration}")
      }
      Self::Condition(condition) => {
        write!(f, "{condition}")
      }
    }
  }
}

struct Declaration<T> {
  variable: T,
  val: Bool,
}

impl<T> FromStr for Declaration<T>
where
  T: FromStr,
  <T as FromStr>::Err: Error + 'static,
{
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let (variable, val) = s.split_once(": ").ok_or_else(|| {
      AocError::Parse(format!(
        "Could not parse Declaration: no \": \" found in {s}"
      ))
    })?;

    Ok(Declaration {
      variable: variable.parse()?,
      val: val.parse()?,
    })
  }
}

impl<T: Display> Display for Declaration<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.variable, self.val)
  }
}

struct Condition<T> {
  op: ConditionOp<T>,
  result: T,
}

impl<T> FromStr for Condition<T>
where
  T: FromStr,
  <T as FromStr>::Err: Error + 'static,
{
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let [lhs, op, rhs, _, res] = iter_to_slice(s.split(" "))?;

    let l = lhs.parse()?;
    let r = rhs.parse()?;
    Ok(Self {
      op: match op {
        "AND" => Ok(ConditionOp::And { l, r }),
        "OR" => Ok(ConditionOp::Or { l, r }),
        "XOR" => Ok(ConditionOp::Xor { l, r }),
        _ => Err(AocError::Parse(format!("Unrecognized op {op}"))),
      }?,
      result: res.parse()?,
    })
  }
}

impl<T: Display> Display for Condition<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.op, self.result)
  }
}

enum ConditionOp<T> {
  And { l: T, r: T },
  Or { l: T, r: T },
  Xor { l: T, r: T },
}

impl<T> ConditionOp<T>
where
  T: BitAnd<T, Output = T> + BitOr<T, Output = T> + BitXor<T, Output = T> + Clone,
{
  fn compute(&self) -> T {
    match self {
      Self::And { l, r } => l.clone() & r.clone(),
      Self::Or { l, r } => l.clone() | r.clone(),
      Self::Xor { l, r } => l.clone() ^ r.clone(),
    }
  }
}

impl<T: Display> Display for ConditionOp<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      ConditionOp::And { l, r } => write!(f, "{l} & {r}"),
      ConditionOp::Or { l, r } => write!(f, "{l} | {r}"),
      ConditionOp::Xor { l, r } => write!(f, "{l} ^ {r}"),
    }
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let contents = read_to_string(INPUT_FILE)?;
  let (bindings, conditions) = contents
    .split_once("\n\n")
    .ok_or_else(|| AocError::Parse(format!("No empty line found in {INPUT_FILE}")))?;

  let bindings: Vec<Declaration<String>> = bindings
    .lines()
    .map(|line| line.parse())
    .collect::<AocResult<_>>()?;
  let conditions: Vec<Condition<String>> = conditions
    .lines()
    .map(|line| line.parse())
    .collect::<AocResult<_>>()?;

  for binding in bindings.iter() {
    println!("{binding}");
  }
  for condition in conditions.iter() {
    println!("{condition}");
  }

  Ok(())
}
