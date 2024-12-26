use std::{collections::HashMap, hash::Hash};

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
