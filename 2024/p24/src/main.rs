use std::{
  array,
  collections::HashMap,
  error::Error,
  fmt::{self, Display, Formatter},
  fs::read_to_string,
  iter::Flatten,
  ops::{BitAnd, BitOr, BitXor},
  str::FromStr,
};

use util::{
  algorithm::{TopologicalOrd, TopologicalSort},
  error::{AocError, AocResult},
  iter_util::iter_to_slice,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Bool {
  val: bool,
}

impl From<bool> for Bool {
  fn from(value: bool) -> Self {
    Bool { val: value }
  }
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

impl<T> TopologicalOrd<T> for Statement<T>
where
  T: Clone,
{
  type DependsOnIter = Flatten<array::IntoIter<Option<T>, 2>>;

  fn key(&self) -> T {
    match self {
      Self::Declaration(declaration) => declaration.variable.clone(),
      Self::Condition(condition) => condition.result.clone(),
    }
  }

  fn depends_on(&self) -> Self::DependsOnIter {
    match self {
      Self::Declaration(_) => [None, None],
      Self::Condition(condition) => [
        Some(condition.op.lhs().clone()),
        Some(condition.op.rhs().clone()),
      ],
    }
    .into_iter()
    .flatten()
  }
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

impl<T> ConditionOp<T> {
  fn lhs(&self) -> &T {
    match self {
      Self::And { l, r: _ } => l,
      Self::Or { l, r: _ } => l,
      Self::Xor { l, r: _ } => l,
    }
  }

  fn rhs(&self) -> &T {
    match self {
      Self::And { l: _, r } => r,
      Self::Or { l: _, r } => r,
      Self::Xor { l: _, r } => r,
    }
  }
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

struct NumberSolver {
  statements: Vec<Statement<usize>>,
  number_bit_idx: Vec<usize>,
}

impl NumberSolver {
  fn new(
    bindings: impl Iterator<Item = Declaration<String>>,
    conditions: impl Iterator<Item = Condition<String>>,
  ) -> AocResult<Self> {
    let sorted = TopologicalSort::new(
      bindings
        .map(Statement::Declaration)
        .chain(conditions.map(Statement::Condition)),
    )
    .pop_all()?;

    let mut name_map = HashMap::new();
    let statements = sorted
      .into_iter()
      .map(|statement| match statement {
        Statement::Declaration(Declaration { variable, val }) => {
          let var_idx = name_map.len();
          name_map.insert(variable, var_idx);
          Statement::Declaration(Declaration {
            variable: var_idx,
            val,
          })
        }
        Statement::Condition(Condition { op, result }) => {
          let &l = name_map.get(op.lhs()).unwrap();
          let &r = name_map.get(op.rhs()).unwrap();

          let result_idx = name_map.len();
          name_map.insert(result, result_idx);

          Statement::Condition(Condition {
            result: result_idx,
            op: match op {
              ConditionOp::And { l: _, r: _ } => ConditionOp::And { l, r },
              ConditionOp::Or { l: _, r: _ } => ConditionOp::Or { l, r },
              ConditionOp::Xor { l: _, r: _ } => ConditionOp::Xor { l, r },
            },
          })
        }
      })
      .collect();

    let mut number_bit_idx: Vec<_> = (0..)
      .map(|idx| format!("z{idx:#02}"))
      .map_while(|z_var| name_map.get(&z_var).cloned())
      .collect();
    number_bit_idx.reverse();

    Ok(Self {
      statements,
      number_bit_idx,
    })
  }

  fn num_variables(&self) -> usize {
    self.statements.len()
  }

  fn solve(&self) -> u64 {
    let mut bindings: Vec<Bool> = vec![false.into(); self.num_variables()];

    for statement in self.statements.iter() {
      match statement {
        Statement::Declaration(Declaration { variable, val }) => {
          bindings[*variable] = *val;
        }
        Statement::Condition(Condition { op, result }) => {
          let l = bindings[*op.lhs()];
          let r = bindings[*op.rhs()];
          bindings[*result] = match op {
            ConditionOp::And { l: _, r: _ } => ConditionOp::And { l, r },
            ConditionOp::Or { l: _, r: _ } => ConditionOp::Or { l, r },
            ConditionOp::Xor { l: _, r: _ } => ConditionOp::Xor { l, r },
          }
          .compute();
        }
      }
    }

    self.number_bit_idx.iter().fold(0, |v, &idx| {
      (v << 1) + if bindings[idx].val { 1 } else { 0 }
    })
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

  let result = NumberSolver::new(bindings.into_iter(), conditions.into_iter())?.solve();
  println!("z result: {result}");

  Ok(())
}
