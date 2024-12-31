use std::{
  collections::{hash_map::Entry, HashMap, HashSet},
  hash::Hash,
};

use crate::error::{AocError, AocResult};

pub fn frequency_map<'a, T, I>(elems: I) -> HashMap<T, u32>
where
  T: Clone + PartialEq + Eq + Hash + 'a,
  I: IntoIterator<Item = &'a T>,
{
  elems.into_iter().fold(HashMap::new(), |mut map, elem| {
    match map.entry(elem.clone()) {
      std::collections::hash_map::Entry::Occupied(mut entry) => {
        *entry.get_mut() += 1;
      }
      std::collections::hash_map::Entry::Vacant(entry) => {
        entry.insert(1);
      }
    };
    map
  })
}

pub trait TopologicalOrd<K> {
  type DependsOnIter: Iterator<Item = K>;

  fn key(&self) -> K;

  fn depends_on(&self) -> Self::DependsOnIter;
}

struct Item<K, T> {
  val: T,
  dependencies: u64,
  depended_by: Vec<K>,
}

pub struct TopologicalSort<K, T> {
  items: HashMap<K, Item<K, T>>,
  free_entries: Vec<K>,
}

impl<K, T> TopologicalSort<K, T>
where
  K: Clone + PartialEq + Eq + Hash,
  T: TopologicalOrd<K>,
{
  pub fn new(iter: impl Iterator<Item = T>) -> Self {
    let mut items = HashMap::new();
    let mut depended_by: HashMap<K, Vec<K>> = HashMap::new();
    let mut free_entries = HashSet::<K>::new();
    for item in iter {
      let item_key = item.key();

      let mut dependency_count = 0;
      for dependency in item.depends_on() {
        dependency_count += 1;
        match depended_by.entry(dependency) {
          Entry::Occupied(mut entry) => {
            entry.get_mut().push(item_key.clone());
          }
          Entry::Vacant(entry) => {
            entry.insert(vec![item_key.clone()]);
          }
        }
      }
      if dependency_count == 0 {
        free_entries.insert(item_key.clone());
      }
      items.insert(item_key, (item, dependency_count));
    }

    let items = items
      .into_iter()
      .map(|(k, (t, dependencies))| {
        let depended_by = depended_by.remove(&k).unwrap_or_default();
        (
          k,
          Item {
            val: t,
            dependencies,
            depended_by,
          },
        )
      })
      .collect();

    Self {
      items,
      free_entries: free_entries.into_iter().collect(),
    }
  }

  pub fn pop(&mut self) -> Option<T> {
    let k = self.free_entries.pop()?;
    let Item {
      val,
      dependencies: _,
      depended_by,
    } = self.items.remove(&k)?;

    for dependency in depended_by {
      match self.items.entry(dependency.clone()) {
        Entry::Occupied(mut entry) => {
          entry.get_mut().dependencies -= 1;
          if entry.get().dependencies == 0 {
            self.free_entries.push(dependency);
          }
        }
        Entry::Vacant(_) => unreachable!(),
      }
    }

    Some(val)
  }

  pub fn pop_all(mut self) -> AocResult<Vec<T>> {
    let num_items = self.items.len();
    let mut sorted_items = Vec::with_capacity(num_items);
    while let Some(item) = self.pop() {
      sorted_items.push(item);
    }
    if sorted_items.len() != num_items {
      return Err(AocError::Runtime("No topological sort found".to_owned()).into());
    }
    Ok(sorted_items)
  }
}

#[cfg(test)]
mod tests {
  use std::iter;

  use crate::algorithm::{TopologicalOrd, TopologicalSort};

  fn nonzero(val: &u32) -> bool {
    *val != 0
  }

  #[test]
  fn test_simple() {
    #[derive(Debug)]
    struct Num {
      val: u32,
    }
    impl TopologicalOrd<u32> for Num {
      type DependsOnIter = std::iter::Filter<std::iter::Once<u32>, for<'a> fn(&'a u32) -> bool>;

      fn key(&self) -> u32 {
        self.val
      }
      fn depends_on(&self) -> Self::DependsOnIter {
        iter::once(self.val - 1).filter(nonzero)
      }
    }

    let sorted = TopologicalSort::new((1..=100).map(|val| Num {
      val: ((val + 4) * 13) % 100 + 1,
    }))
    .pop_all()
    .unwrap();

    assert!(sorted.into_iter().map(|num| num.val).eq(1..=100));
  }

  #[test]
  fn test_many_deps() {
    #[derive(Debug)]
    struct Num {
      val: u32,
    }
    impl TopologicalOrd<u32> for Num {
      type DependsOnIter = std::ops::Range<u32>;

      fn key(&self) -> u32 {
        self.val
      }
      fn depends_on(&self) -> Self::DependsOnIter {
        1..self.val
      }
    }

    let sorted = TopologicalSort::new((1..=100).map(|val| Num {
      val: ((val + 4) * 13) % 100 + 1,
    }))
    .pop_all()
    .unwrap();

    assert!(sorted.into_iter().map(|num| num.val).eq(1..=100));
  }
}
