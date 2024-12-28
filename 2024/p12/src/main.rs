use std::ops::Add;

use util::{
  error::AocResult,
  grid::{Grid, Pos},
  parse::parse_grid,
  union_find::UnionFind,
};

#[derive(Clone)]
struct AreaPerim {
  area: u32,
  perim: u32,
}

impl AreaPerim {
  fn price(&self) -> u32 {
    self.area * self.perim
  }
}

impl Add for AreaPerim {
  type Output = Self;

  fn add(self, rhs: Self) -> Self::Output {
    Self {
      area: self.area + rhs.area,
      perim: self.perim + rhs.perim,
    }
  }
}

impl Default for AreaPerim {
  fn default() -> Self {
    Self { area: 1, perim: 4 }
  }
}

fn fence_price(grid: &Grid) -> u32 {
  let mut uf = UnionFind::<Pos, AreaPerim>::from_keys(grid.positions());
  for (pos, plant) in grid.iter() {
    for neighbor in grid.top_left_orthogonal_neighbors(pos) {
      if grid[neighbor] == plant {
        let key = uf.union(pos, neighbor);
        uf.metadata_mut(key).perim -= 2;
      }
    }
  }

  uf.root_level_keys()
    .into_iter()
    .map(|key| uf.metadata(key).price())
    .sum()
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let grid = parse_grid(INPUT_FILE)?;

  let price = fence_price(&grid);
  println!("Price: {price}");

  Ok(())
}
