use std::{
  collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
  error::Error,
  fmt::{self, Debug, Display, Formatter},
  fs::read_to_string,
  hash::Hash,
  iter,
  str::FromStr,
};

use itertools::Itertools;
use util::error::{AocError, AocResult};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Name {
  name: u32,
}

impl Name {
  fn first_letter(&self) -> u8 {
    (self.name / 26) as u8 + b'a'
  }

  fn second_letter(&self) -> u8 {
    (self.name % 26) as u8 + b'a'
  }
}

impl Debug for Name {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

impl Display for Name {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}{}",
      self.first_letter() as char,
      self.second_letter() as char
    )
  }
}

impl FromStr for Name {
  type Err = Box<dyn Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s.len() != 2 {
      return Err(AocError::Parse(format!("\"{s}\" is not a 2-letter name")).into());
    }

    let d1 = s.as_bytes()[0];
    let d2 = s.as_bytes()[1];
    if !d1.is_ascii_alphabetic() || !d2.is_ascii_alphabetic() {
      return Err(AocError::Parse(format!("\"{s}\" is not a 2-letter alphabetic name")).into());
    }

    Ok(Self {
      name: (d1 - b'a') as u32 * 26 + (d2 - b'a') as u32,
    })
  }
}

fn interconnected_sets_of_3_with_t(connections: &HashMap<Name, HashSet<Name>>) -> u64 {
  connections
    .iter()
    .flat_map(|(&k, k_connections)| {
      k_connections
        .iter()
        .combinations(2)
        .filter(move |combo| {
          let conn1 = combo[0];
          let conn2 = combo[1];
          connections
            .get(conn1)
            .is_some_and(|conn| conn.contains(&k) && conn.contains(conn2))
            && connections
              .get(conn2)
              .is_some_and(|conn| conn.contains(&k) && conn.contains(conn1))
        })
        .map(move |combo| [k, *combo[0], *combo[1]])
    })
    .map(|mut elems| {
      elems.sort();
      elems
    })
    .filter(|elems| elems.iter().any(|name| name.first_letter() == b't'))
    .count() as u64
    / 3
}

fn next_interconnected_sets(
  connections: &HashMap<Name, HashSet<Name>>,
  interconnections: &HashMap<Name, HashSet<BTreeSet<Name>>>,
) -> HashMap<Name, HashSet<BTreeSet<Name>>> {
  interconnections
    .iter()
    .map(|(&k, k_connections)| {
      (
        k,
        k_connections
          .iter()
          .flat_map(move |combo| {
            connections.get(&k).map(|single_connections| {
              single_connections
                .iter()
                .filter(|name| !combo.contains(name))
                .filter(|name| {
                  connections.get(name).is_some_and(|name_connections| {
                    combo
                      .iter()
                      .all(|combo_name| name_connections.contains(combo_name))
                  })
                })
                .map(|&connected_name| {
                  let mut new_combo = (*combo).clone();
                  new_combo.insert(connected_name);
                  new_combo
                })
            })
          })
          .flatten()
          .collect::<HashSet<_>>(),
      )
    })
    .filter(|(_, conn)| !conn.is_empty())
    .collect()
}

fn largest_interconnected_set(
  connections: &HashMap<Name, HashSet<Name>>,
) -> Option<impl Iterator<Item = Name>> {
  let largest_set = iter::successors(
    Some(
      connections
        .iter()
        .map(|(&name, connections)| {
          (
            name,
            connections
              .iter()
              .map(|&connection| [connection].into_iter().collect::<BTreeSet<_>>())
              .collect(),
          )
        })
        .collect(),
    ),
    |interconnections: &HashMap<Name, HashSet<BTreeSet<Name>>>| {
      println!("Interconn: {interconnections:?}");
      if interconnections.len() <= 1 {
        None
      } else {
        Some(next_interconnected_sets(connections, interconnections))
      }
    },
  )
  .last()
  .unwrap();

  if let Some((name, connections)) = largest_set.into_iter().next() {
    if let Some(names) = connections.into_iter().next() {
      return Some(names.into_iter().chain([name]));
    }
  }

  None
}

fn make_connection<T>(map: &mut HashMap<T, HashSet<T>>, t1: T, t2: T)
where
  T: Clone + Hash + PartialEq + Eq,
{
  let mut insert = |t1: T, t2: T| match map.entry(t1) {
    Entry::Occupied(mut entry) => {
      entry.get_mut().insert(t2);
    }
    Entry::Vacant(entry) => {
      entry.insert([t2].into());
    }
  };

  insert(t1.clone(), t2.clone());
  insert(t2, t1);
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input2.txt";
  let connections = read_to_string(INPUT_FILE)?
    .lines()
    .map(|line| -> AocResult<_> {
      let (from_name, to_name) = line
        .split_once('-')
        .ok_or_else(|| AocError::Parse(format!("Line \"{line}\" does not contain a '-'")))?;
      Ok((from_name.parse::<Name>()?, to_name.parse::<Name>()?))
    })
    .try_fold(HashMap::new(), |mut map, names| -> AocResult<_> {
      let (name1, name2) = names?;
      make_connection(&mut map, name1, name2);
      Ok(map)
    })?;

  let num_with_t = interconnected_sets_of_3_with_t(&connections);
  println!("Sets of 3 with t-name: {num_with_t}");

  if let Some(largest_set) = largest_interconnected_set(&connections) {
    println!("Password: {}", largest_set.sorted().join(","));
  } else {
    println!("No password found :(");
  }

  Ok(())
}
