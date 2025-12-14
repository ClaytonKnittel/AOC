use std::{fmt::Debug, str::FromStr};

use itertools::Itertools;
use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

fn strip_parens(s: &str, open: char, close: char) -> Result<&str, AocError> {
  s.strip_prefix(open)
    .ok_or_else(|| AocError::Parse(format!("Failed to parse \"{s}\": missing '{open}' prefix")))?
    .strip_suffix(close)
    .ok_or_else(|| AocError::Parse(format!("Failed to parse \"{s}\": missing '{close}' suffix")))
}

struct Target {
  sequence: u32,
  length: u32,
}

impl FromStr for Target {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let target = strip_parens(s, '[', ']')?;

    if target.len() > u32::BITS as usize {
      return Err(AocError::Parse(format!(
        "Target sequence \"{s}\" is length {}, max supported length is {}",
        target.len(),
        u32::BITS
      )));
    }

    let (sequence, length) =
      target
        .chars()
        .enumerate()
        .try_fold((0, 0), |(acc, len), (idx, target)| match target {
          '#' => Ok((acc | (1 << idx), len + 1)),
          '.' => Ok((acc, len + 1)),
          _ => Err(AocError::Parse(format!(
            "Unexpected character {target} in target sequence \"{s}\""
          ))),
        })?;

    Ok(Self { sequence, length })
  }
}

impl Debug for Target {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[")?;
    for d in 0..self.length {
      if (self.sequence & (1 << d)) != 0 {
        write!(f, "#")?;
      } else {
        write!(f, ".")?;
      }
    }
    write!(f, "]")
  }
}

struct Button {
  sequence: u32,
}

impl FromStr for Button {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let toggles = strip_parens(s, '(', ')')?;

    let sequence = toggles.split(',').try_fold(0, |acc, toggle| {
      let toggle = toggle.parse::<u8>().map_err(|_| {
        AocError::Parse(format!(
          "Failed to parse \"{toggle}\" as u8 for button string \"{s}\""
        ))
      })?;

      if toggle >= u32::BITS as u8 {
        return Err(AocError::Parse(format!(
          "\"{s}\" contains out of range toggle {toggle}"
        )));
      }

      let bit_toggle = 1 << toggle;
      if (bit_toggle & acc) != 0 {
        return Err(AocError::Parse(format!(
          "\"{s}\" contains duplicate toggle {toggle}"
        )));
      }

      Ok(acc | bit_toggle)
    })?;

    Ok(Self { sequence })
  }
}

impl Debug for Button {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "({:?})",
      (0..u32::BITS)
        .filter(|i| (self.sequence & (1 << i)) != 0)
        .collect_vec()
    )
  }
}

struct JoltageRequirements {
  requirements: Vec<u32>,
}

impl FromStr for JoltageRequirements {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let requirements = strip_parens(s, '{', '}')?;

    let requirements = requirements
      .split(',')
      .map(|requirement| {
        requirement.parse().map_err(|_| {
          AocError::Parse(format!(
            "Failed to parse \"{requirement}\" as u8 in joltage requirements string \"{s}\""
          ))
        })
      })
      .collect::<Result<_, _>>()?;

    Ok(Self { requirements })
  }
}

impl Debug for JoltageRequirements {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{{{:?}}}", self.requirements)
  }
}

#[derive(Debug)]
struct Machine {
  target: Target,
  buttons: Vec<Button>,
  joltage_requirements: JoltageRequirements,
}

impl FromStr for Machine {
  type Err = AocError;

  fn from_str(s: &str) -> Result<Self, AocError> {
    let mut words = s.split(' ');
    let target = words
      .next()
      .ok_or_else(|| AocError::Parse("Unexpected empty machine descriptor".to_owned()))?
      .parse()?;

    let mut buttons = Vec::new();
    let joltage_requirements = loop {
      let Some(button) = words.next() else {
        return Err(AocError::Parse(format!(
          "Machine descriptor \"{s}\" is missing joltage requirements"
        )));
      };

      if button.starts_with('{') {
        break button;
      }

      buttons.push(button.parse()?);
    };

    Ok(Self {
      target,
      buttons,
      joltage_requirements: joltage_requirements.parse()?,
    })
  }
}

fn fewest_presses_to_configure(machine: &Machine) -> AocResult<u64> {
  if machine.target.sequence == 0 {
    return Ok(0);
  }

  let mut visited_sequences = vec![false; 1 << machine.target.length];

  let mut pressed_buttons = vec![(0u32, 0)];
  let mut total_presses = 0;
  let mut next_pressed_buttons = Vec::new();
  loop {
    for (buttons, sequence) in pressed_buttons.iter() {
      for idx in (0..machine.buttons.len()).rev() {
        let next_button = 1 << idx;
        if next_button <= *buttons {
          break;
        }

        let next_buttons = buttons + next_button;
        let next_sequence = sequence ^ machine.buttons[idx].sequence;

        if next_sequence == machine.target.sequence {
          return Ok(total_presses + 1);
        }
        if visited_sequences[next_sequence as usize] {
          continue;
        }

        next_pressed_buttons.push((next_buttons, next_sequence));
        visited_sequences[next_sequence as usize] = true;
      }
    }

    if next_pressed_buttons.is_empty() {
      return Err(
        AocError::Runtime(format!("Target sequence is unreachable for {machine:?}")).into(),
      );
    }

    std::mem::swap(&mut pressed_buttons, &mut next_pressed_buttons);
    next_pressed_buttons.clear();
    total_presses += 1;
  }
}

pub struct P10;

impl NumericSolution for P10 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let v: Vec<_> = list_of_strings(input_path)?
      .map(|line| -> AocResult<Machine> { Ok(line?.parse()?) })
      .collect::<Result<_, _>>()?;

    v.iter()
      .map(fewest_presses_to_configure)
      .try_fold(0, |acc, presses| Ok(acc + presses?))
  }
}
