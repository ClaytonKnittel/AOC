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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
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
  name_lookup: Vec<String>,
}

impl NumberSolver {
  fn x(idx: usize) -> String {
    format!("x{idx:#02}")
  }

  fn y(idx: usize) -> String {
    format!("y{idx:#02}")
  }

  fn z(idx: usize) -> String {
    format!("z{idx:#02}")
  }

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

    // let var_bindings: HashMap<_, _> = sorted
    //   .iter()
    //   .flat_map(|statement| {
    //     if let Statement::Condition(Condition { op, result }) = statement {
    //       Some((result.clone(), op))
    //     } else {
    //       None
    //     }
    //   })
    //   .collect();

    // fn expand_op(
    //   op: &ConditionOp<String>,
    //   var_bindings: &HashMap<String, &ConditionOp<String>>,
    // ) -> String {
    //   let l = var_bindings
    //     .get(op.lhs())
    //     .map(|statement| expand_op(statement, var_bindings))
    //     .unwrap_or(op.lhs().to_owned());
    //   let r = var_bindings
    //     .get(op.rhs())
    //     .map(|statement| expand_op(statement, var_bindings))
    //     .unwrap_or(op.rhs().to_owned());
    //   let op = match op {
    //     ConditionOp::And { l: _, r: _ } => ConditionOp::And { l, r },
    //     ConditionOp::Or { l: _, r: _ } => ConditionOp::Or { l, r },
    //     ConditionOp::Xor { l: _, r: _ } => ConditionOp::Xor { l, r },
    //   };
    //   format!("({op})")
    // }

    // fn expand_statement(
    //   statement: &Statement<String>,
    //   var_bindings: &HashMap<String, &ConditionOp<String>>,
    // ) -> String {
    //   match statement {
    //     Statement::Declaration(declaration) => format!("{declaration}"),
    //     Statement::Condition(Condition { op, result }) => {
    //       format!("{} = {result}", expand_op(op, var_bindings))
    //     }
    //   }
    // }

    // for statement in sorted.iter() {
    //   println!("{statement}");
    //   println!("{}", expand_statement(statement, &var_bindings));
    // }

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

    let mut name_lookup = vec!["".to_string(); name_map.len()];
    for (name, &idx) in name_map.iter() {
      name_lookup[idx] = name.clone();
    }

    let mut number_bit_idx: Vec<_> = (0..)
      .map(|idx| format!("z{idx:#02}"))
      .map_while(|z_var| name_map.get(&z_var).cloned())
      .collect();
    number_bit_idx.reverse();

    Ok(Self {
      statements,
      number_bit_idx,
      name_lookup,
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

  fn find_wire_swaps(&self) -> AocResult<impl Iterator<Item = [String; 2]>> {
    let op_lookup: HashMap<_, _> = self
      .statements
      .iter()
      .flat_map(|statement| {
        if let Statement::Condition(Condition { op, result }) = statement {
          Some((op.clone(), *result))
        } else {
          None
        }
      })
      .collect();
    // Map from variable name to the index of the statement it is defined by.
    let result_lookup: HashMap<_, _> = self
      .statements
      .iter()
      .enumerate()
      .map(|(idx, statement)| {
        (
          self.name_lookup[match statement {
            Statement::Declaration(Declaration { variable, val: _ }) => *variable,
            Statement::Condition(Condition { op: _, result }) => *result,
          }]
          .clone(),
          idx,
        )
      })
      .collect();

    fn lookup(
      cond: &ConditionOp<usize>,
      op_lookup: &HashMap<ConditionOp<usize>, usize>,
    ) -> Option<usize> {
      op_lookup
        .get(cond)
        .or_else(|| {
          op_lookup.get(&match cond.clone() {
            ConditionOp::And { l, r } => ConditionOp::And { l: r, r: l },
            ConditionOp::Or { l, r } => ConditionOp::Or { l: r, r: l },
            ConditionOp::Xor { l, r } => ConditionOp::Xor { l: r, r: l },
          })
        })
        .cloned()
    }

    let mut prev_carry = None;
    for bit_idx in 0..self.number_bit_idx.len() {
      println!("Gonna get {}", Self::z(bit_idx));

      if bit_idx < self.number_bit_idx.len() - 1 {
        let xc = *result_lookup.get(&Self::x(bit_idx)).unwrap();
        let yc = *result_lookup.get(&Self::y(bit_idx)).unwrap();
        let from_cur_bits = ConditionOp::Xor { l: xc, r: yc };
        let from_cur_bits_idx = lookup(&from_cur_bits, &op_lookup);
        println!(
          "{} = {:?}",
          from_cur_bits,
          from_cur_bits_idx.map(|name_idx| &self.name_lookup[name_idx])
        );
      }

      if bit_idx > 0 {
        let xp = *result_lookup.get(&Self::x(bit_idx - 1)).unwrap();
        let yp = *result_lookup.get(&Self::y(bit_idx - 1)).unwrap();
        let from_prev_bits = ConditionOp::And { l: xp, r: yp };
        let from_prev_bits_idx = lookup(&from_prev_bits, &op_lookup).unwrap();
        println!("{} = {:?}", from_prev_bits, from_prev_bits_idx);

        let mut carry_idx = from_prev_bits_idx;

        if let Some(pc) = prev_carry {
          let either_prev = ConditionOp::Xor { l: xp, r: yp };
          let either_prev_idx = lookup(&either_prev, &op_lookup).unwrap();
          let from_carry = ConditionOp::And {
            l: either_prev_idx,
            r: pc,
          };
          let from_carry_idx = lookup(&from_carry, &op_lookup).unwrap();

          println!("{} = {:?}", either_prev, either_prev_idx);
          println!("{} = {:?}", from_carry, from_carry_idx);

          let carry = ConditionOp::Or {
            l: from_prev_bits_idx,
            r: from_carry_idx,
          };
          carry_idx = lookup(&carry, &op_lookup).unwrap();
          println!("{} = {:?}", carry, carry_idx);
        }

        prev_carry = Some(carry_idx);
      }
    }

    Ok(std::iter::empty())
  }
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input3.txt";
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

  let solver = NumberSolver::new(bindings.into_iter(), conditions.into_iter())?;

  let result = solver.solve();
  println!("z result: {result}");

  let mut wire_swaps: Vec<_> = solver.find_wire_swaps()?.flatten().collect();
  wire_swaps.sort();
  println!("Swapped wires: {}", wire_swaps.join(","));

  Ok(())
}
