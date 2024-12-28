use std::ops::Add;

use util::{
  error::AocResult,
  grid::{Grid, Pos},
  parse::parse_grid,
  union_find::UnionFind,
};

#[derive(Clone, Debug)]
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

fn discount_fence_price(grid: &Grid) -> u32 {
  let mut uf = UnionFind::<Pos, AreaPerim>::from_keys(grid.positions());
  for (pos, plant) in grid.iter() {
    let top_pos = Pos {
      row: pos.row.wrapping_sub(1),
      ..pos
    };
    let left_pos = Pos {
      col: pos.col.wrapping_sub(1),
      ..pos
    };

    let top_neighbor = pos.row != 0 && grid[top_pos] == plant;
    let left_neighbor = pos.col != 0 && grid[left_pos] == plant;

    let top_left_neighbor = pos.row != 0
      && pos.col != 0
      && grid[Pos {
        row: pos.row - 1,
        col: pos.col - 1,
      }] == plant;
    let top_right_neighbor = pos.row != 0
      && pos.col != grid.width() - 1
      && grid[Pos {
        row: pos.row - 1,
        col: pos.col + 1,
      }] == plant;

    let mut key = pos;
    if top_neighbor {
      key = uf.union(pos, top_pos);
    }
    if left_neighbor {
      key = uf.union(pos, left_pos);
    }
    let metadata = uf.metadata_mut(key);

    metadata.perim -= match (
      top_neighbor,
      left_neighbor,
      top_left_neighbor,
      top_right_neighbor,
    ) {
      // _ . _
      // . O .
      (false, false, _, _) => 0,
      // . . .
      // X O .
      (false, true, false, _) => 4,
      // X . .
      // X O .
      (false, true, true, _) => 2,
      // . X .
      // . O .
      (true, false, false, false) => 4,
      // . X X
      // . O .
      (true, false, false, true) => 2,
      // X X .
      // . O .
      (true, false, true, false) => 2,
      // X X X
      // . O .
      (true, false, true, true) => 0,
      // _ X .
      // X O .
      (true, true, _, false) => 6,
      // _ X X
      // X O .
      (true, true, _, true) => 4,
    };
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

  let discount_price = discount_fence_price(&grid);
  println!("Discount price: {discount_price}");

  Ok(())
}
