use std::{
  collections::{hash_map::Entry, HashMap},
  fs::read_to_string,
};

use itertools::Itertools;
use util::error::AocResult;

const NUM_ITERS: u64 = 2000;

fn next(secret: u64) -> u64 {
  fn mix(s: u64, v: u64) -> u64 {
    s ^ v
  }
  fn prune(s: u64) -> u64 {
    s % 16777216
  }

  let a = prune(mix(secret * 64, secret));
  let b = prune(mix(a / 32, a));
  prune(mix(b * 2048, b))
}

#[derive(PartialEq, Eq, Hash)]
struct PriceChange {
  changes: (i8, i8, i8, i8),
}

fn best_buy_sequence_profit(secrets: &[u64]) -> u64 {
  let mut price_map = HashMap::<PriceChange, u64>::new();

  for &secret in secrets {
    let new_prices_map = (0..NUM_ITERS)
      .scan(secret, |secret, _| {
        *secret = next(*secret);
        Some(*secret)
      })
      .map(|secret| (secret % 10) as i8)
      .tuple_windows()
      .map(|(price1, price2)| (price2, price2 - price1))
      .tuple_windows::<(_, _, _, _)>()
      .map(|((_, c1), (_, c2), (_, c3), (price, c4))| {
        (
          PriceChange {
            changes: (c1, c2, c3, c4),
          },
          price as u64,
        )
      })
      .collect::<Vec<_>>()
      .into_iter()
      .rev()
      .collect::<HashMap<_, _>>();

    for (changes, price) in new_prices_map {
      match price_map.entry(changes) {
        Entry::Occupied(mut entry) => {
          *entry.get_mut() += price;
        }
        Entry::Vacant(entry) => {
          entry.insert(price);
        }
      }
    }
  }

  price_map
    .into_iter()
    .max_by(|(_, profit1), (_, profit2)| profit1.cmp(profit2))
    .unwrap()
    .1
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let secrets: Vec<u64> = read_to_string(INPUT_FILE)?
    .lines()
    .map(|line| Ok(line.parse()?))
    .collect::<AocResult<_>>()?;

  let sum_of_secrets: u64 = secrets
    .iter()
    .map(|&secret| (0..NUM_ITERS).fold(secret, |secret, _| next(secret)))
    .sum();
  println!("Sum of {NUM_ITERS}th secrets: {sum_of_secrets}");

  let highest_profit = best_buy_sequence_profit(&secrets);
  println!("Highest profit from monkey: {highest_profit}");

  Ok(())
}
