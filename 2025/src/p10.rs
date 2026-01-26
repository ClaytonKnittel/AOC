use std::{fmt::Debug, iter::successors, str::FromStr};

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

fn iter_ones(n: u32) -> impl Iterator<Item = u32> {
  let if_ne_zero = |value: u32| (value != 0).then_some(value);
  successors(if_ne_zero(n), move |&value| {
    let value = value & (value - 1);
    if_ne_zero(value)
  })
  .map(|mask| mask.trailing_zeros())
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

impl Button {
  fn toggles(&self) -> impl Iterator<Item = u32> {
    iter_ones(self.sequence)
  }
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

    let mut buttons = Vec::<Button>::new();
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

    // Sort buttons from most affected targets to least.
    buttons.sort_by_key(|button| button.sequence.count_zeros());

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

enum ConfigureResult {
  Success(u64),
  Failure,
  LackingCoverage,
}

fn configure_joltage(joltage_requirements: &[u32], buttons: &[Button]) -> ConfigureResult {
  if buttons.is_empty() {
    if joltage_requirements
      .iter()
      .all(|&requirement| requirement == 0)
    {
      return ConfigureResult::Success(0);
    } else {
      return ConfigureResult::LackingCoverage;
    }
  }

  let missing_buttons_mask = !buttons.iter().fold(0, |acc, button| acc | button.sequence)
    & ((1 << joltage_requirements.len()) - 1);
  if iter_ones(missing_buttons_mask).any(|idx| joltage_requirements[idx as usize] != 0) {
    return ConfigureResult::LackingCoverage;
  }

  let largest_button = &buttons[0];
  let max_presses = largest_button
    .toggles()
    .map(|toggle| joltage_requirements[toggle as usize])
    .min()
    .unwrap();

  let mut new_joltage_requirements: Vec<_> = joltage_requirements.into();
  for toggle in largest_button.toggles() {
    new_joltage_requirements[toggle as usize] -= max_presses;
  }

  let mut min = None;
  for presses in (0..=max_presses).rev() {
    match configure_joltage(&new_joltage_requirements, &buttons[1..]) {
      ConfigureResult::Success(remaining_presses) => {
        let presses = presses as u64 + remaining_presses;
        min = Some(min.map_or(presses, |min: u64| min.min(presses)))
      }
      ConfigureResult::Failure => {}
      ConfigureResult::LackingCoverage => break,
    }

    for toggle in largest_button.toggles() {
      new_joltage_requirements[toggle as usize] += 1;
    }
  }

  match min {
    Some(min_presses) => ConfigureResult::Success(min_presses),
    None => ConfigureResult::Failure,
  }
}

fn fewest_presses_to_configure_joltage(machine: &Machine) -> AocResult<u64> {
  println!("Processing {machine:?}");
  match configure_joltage(&machine.joltage_requirements.requirements, &machine.buttons) {
    ConfigureResult::Success(presses) => Ok(presses),
    _ => Err(
      AocError::Runtime(format!(
        "No valid joltage configuration found for {machine:?}"
      ))
      .into(),
    ),
  }
}

pub struct P10;

impl NumericSolution for P10 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let v: Vec<_> = list_of_strings(input_path)?
      .map(|line| Ok(line?.parse()?))
      .collect::<AocResult<_>>()?;

    v.iter()
      .map(match part {
        Part::P1 => fewest_presses_to_configure,
        Part::P2 => fewest_presses_to_configure_joltage,
      })
      .try_fold(0, |acc, presses| Ok(acc + presses?))
  }
}
