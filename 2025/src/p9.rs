use std::{fmt::Debug, str::FromStr};

use itertools::Itertools;
use util::{
  error::{AocError, AocResult},
  parse::list_of_strings,
};

use crate::solution::{NumericSolution, Part};

/// Describes the orientation of the curve. Right-handed orientation means the
/// inside of the curve is to the right of the line being drawn, when
/// traversing the curve in the order given.
#[derive(Debug, PartialEq, Eq)]
enum Orientation {
  Right,
  Left,
}

fn orientation(points: &[Coord]) -> AocResult<Orientation> {
  match points
    .iter()
    .cycle()
    .tuple_windows()
    .take(points.len())
    .map(|(a, b, c)| (b.delta(a), c.delta(b)))
    .map(|(da, db)| da.x * db.y - da.y * db.x)
    .sum::<i32>()
  {
    4 => Ok(Orientation::Left),
    -4 => Ok(Orientation::Right),
    sum => Err(
      AocError::Runtime(format!(
        "Invalid total rotation {sum}, does the curve intersect itself?"
      ))
      .into(),
    ),
  }
}

#[derive(Clone, Copy)]
struct Coord {
  x: i32,
  y: i32,
}

impl Coord {
  fn min(&self, other: &Self) -> Self {
    Self {
      x: self.x.min(other.x),
      y: self.y.min(other.y),
    }
  }

  fn inc_x(&self, dx: i32) -> Self {
    Self {
      x: self.x + dx,
      y: self.y,
    }
  }

  fn inc_y(&self, dy: i32) -> Self {
    Self {
      x: self.x,
      y: self.y + dy,
    }
  }

  fn inc_to_excl(&self) -> Self {
    Self {
      x: self.x.saturating_add(1),
      y: self.y.saturating_add(1),
    }
  }

  fn max(&self, other: &Self) -> Self {
    Self {
      x: self.x.max(other.x),
      y: self.y.max(other.y),
    }
  }

  fn bb(&self, other: &Self) -> Rect {
    Rect {
      ll: self.min(other),
      ur: self.max(other).inc_to_excl(),
    }
  }

  fn delta(&self, other: &Self) -> Coord {
    let dx = self.x - other.x;
    let dy = self.y - other.y;
    if dx == 0 {
      debug_assert_ne!(dy, 0);
      if dy > 0 {
        Coord { x: 0, y: 1 }
      } else {
        Coord { x: 0, y: -1 }
      }
    } else {
      debug_assert_eq!(dy, 0);
      if dx > 0 {
        Coord { x: 1, y: 0 }
      } else {
        Coord { x: -1, y: 0 }
      }
    }
  }

  fn turn(&self, prev: &Self, next: &Self) -> Orientation {
    let px = self.x - prev.x;
    let py = self.y - prev.y;
    let nx = next.x - self.x;
    let ny = next.y - self.y;
    if px * ny - py * nx > 0 {
      Orientation::Left
    } else {
      Orientation::Right
    }
  }

  fn area_coverage(&self, next: &Self, turn: Orientation) -> AreaCoverage {
    debug_assert!((self.x == next.x) != (self.y == next.y));
    println!(
      "Dir: {}",
      match turn {
        Orientation::Left => "left",
        Orientation::Right => "right",
      }
    );

    let dx = next.x - self.x;
    let dy = next.y - self.y;
    let mut next_corner = *next;
    let mut pos_corner = *self;
    let mut neg_corner = *self;
    if dx < 0 {
      next_corner.y += 1;
      pos_corner.x += 1;
      neg_corner.x += 1;
      pos_corner.y = i32::MIN;
      neg_corner.y = i32::MAX;
      match turn {
        Orientation::Right => pos_corner.x = i32::MAX,
        Orientation::Left => neg_corner.x = i32::MAX,
      }
    } else if dx > 0 {
      next_corner.x += 1;
      pos_corner.y = i32::MAX;
      neg_corner.y = i32::MIN;
      match turn {
        Orientation::Right => pos_corner.x = i32::MIN,
        Orientation::Left => neg_corner.x = i32::MIN,
      }
    } else if dy < 0 {
      pos_corner.y += 1;
      neg_corner.y += 1;
      pos_corner.x = i32::MAX;
      neg_corner.x = i32::MIN;
      match turn {
        Orientation::Right => pos_corner.y = i32::MAX,
        Orientation::Left => neg_corner.y = i32::MAX,
      }
    } else {
      next_corner.x += 1;
      next_corner.y += 1;
      pos_corner.x = i32::MIN;
      neg_corner.x = i32::MAX;
      match turn {
        Orientation::Right => pos_corner.y = i32::MIN,
        Orientation::Left => neg_corner.y = i32::MIN,
      }
    }

    AreaCoverage {
      positive: Rect::from_any_corners(pos_corner, next_corner),
      negative: Rect::from_any_corners(neg_corner, next_corner),
    }
  }
}

impl FromStr for Coord {
  type Err = Box<dyn std::error::Error>;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let (x, y) = s
      .split_once(',')
      .ok_or_else(|| AocError::Parse(format!("Expected \"x,y\", found \"{s}\"")))?;

    Ok(Self {
      x: x.parse()?,
      y: y.parse()?,
    })
  }
}

impl Debug for Coord {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({},{})", self.x, self.y)
  }
}

struct AreaCoverage {
  positive: Rect,
  negative: Rect,
}

struct Rect {
  /// Inclusive
  ll: Coord,
  /// Exclusive
  ur: Coord,
}

impl Rect {
  fn from_any_corners(c1: Coord, c2: Coord) -> Self {
    Self {
      ll: c1.min(&c2),
      ur: c1.max(&c2),
    }
  }

  fn area(&self) -> u64 {
    let dx = (self.ur.x - self.ll.x) as u64;
    let dy = (self.ur.y - self.ll.y) as u64;
    dx * dy
  }

  fn intersection(&self, other: &Rect) -> Rect {
    let ll = Coord {
      x: self.ll.x.max(other.ll.x),
      y: self.ll.y.max(other.ll.y),
    };
    let ur = Coord {
      x: self.ur.x.min(other.ur.x),
      y: self.ur.y.min(other.ur.y),
    }
    .max(&ll);
    Rect { ll, ur }
  }
}

impl Debug for Rect {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({:?}, {:?})", self.ll, self.ur)
  }
}

pub struct P9;

impl NumericSolution for P9 {
  fn solve(input_path: &str, part: Part) -> AocResult<u64> {
    let red_tiles: Vec<Coord> = list_of_strings(input_path)?
      .map(|line| line?.parse())
      .collect::<Result<_, _>>()?;

    match part {
      Part::P1 => red_tiles
        .into_iter()
        .tuple_combinations()
        .map(|(tile1, tile2)| tile1.bb(&tile2).area())
        .max()
        .ok_or_else(|| AocError::Parse("Unexpected empty input".to_owned()).into()),
      Part::P2 => {
        debug_assert!(
          orientation(&red_tiles).is_ok_and(|orientation| orientation == Orientation::Left),
          "{:?}",
          orientation(&red_tiles)
        );

        let rects = red_tiles
          .iter()
          .cycle()
          .tuple_windows()
          .take(red_tiles.len())
          .inspect(|(_, b, c)| {
            println!("{b:?} -> {c:?}");
          })
          .map(|(a, b, c)| b.area_coverage(c, b.turn(a, c)))
          .inspect(|AreaCoverage { positive, negative }| {
            println!("+{positive:?}, -{negative:?}");
          })
          .collect_vec();

        let x_min = red_tiles.iter().map(|tile| tile.x).min().unwrap();
        let x_max = red_tiles.iter().map(|tile| tile.x).max().unwrap();
        let y_min = red_tiles.iter().map(|tile| tile.y).min().unwrap();
        let y_max = red_tiles.iter().map(|tile| tile.y).max().unwrap();

        for (AreaCoverage { positive, negative }, (_, p1, p2)) in
          rects.iter().zip(red_tiles.iter().cycle().tuple_windows())
        {
          println!("{p1:?} -> {p2:?}");
          let r = Rect::from_any_corners(p1.min(p2), p1.max(p2).inc_to_excl());
          for y in (y_min - 1..=y_max + 1).rev() {
            print!("{y:3}:");
            for x in x_min - 1..=x_max + 1 {
              let p = Rect::from_any_corners(Coord { x, y }, Coord { x: x + 1, y: y + 1 });
              let c = r.intersection(&p).area();
              print!("{c:3}");
            }
            println!();
          }
          print!("    ");
          for x in x_min - 1..=x_max + 1 {
            print!("{x:3}");
          }
          println!();
          println!();

          for y in (y_min - 1..=y_max + 1).rev() {
            print!("{y:3}:");
            for x in x_min - 1..=x_max + 1 {
              let p = Rect::from_any_corners(Coord { x, y }, Coord { x: x + 1, y: y + 1 });
              let c =
                positive.intersection(&p).area() as i32 - negative.intersection(&p).area() as i32;
              print!("{c:3}");
            }
            println!();
          }
          print!("    ");
          for x in x_min - 1..=x_max + 1 {
            print!("{x:3}");
          }
          println!();
          println!();
          println!();
        }

        for y in (y_min - 1..=y_max + 1).rev() {
          print!("{y:3}:");
          for x in x_min - 1..=x_max + 1 {
            let p = Rect::from_any_corners(Coord { x, y }, Coord { x: x + 1, y: y + 1 });
            let c = rects
              .iter()
              .map(|AreaCoverage { positive, negative }| {
                positive.intersection(&p).area() as i32 - negative.intersection(&p).area() as i32
              })
              .sum::<i32>();
            print!("{c:3}");
          }
          println!();
        }
        print!("    ");
        for x in x_min - 1..=x_max + 1 {
          print!("{x:3}");
        }
        println!();
        println!();

        let mut bbs = red_tiles
          .into_iter()
          .tuple_combinations()
          .map(|(tile1, tile2)| tile1.bb(&tile2))
          .collect_vec();
        bbs.sort_by_key(|bb| -(bb.area() as i64));

        bbs
          .into_iter()
          .filter(|bb| {
            let rect_contrib = rects
              .iter()
              .map(|AreaCoverage { positive, negative }| {
                positive.intersection(bb).area() as i64 - negative.intersection(bb).area() as i64
              })
              .sum::<i64>();
            debug_assert!(
              rect_contrib == 4 * bb.area() as i64 || rect_contrib == -4 * bb.area() as i64,
              "Expected {} or -{}, got {rect_contrib}",
              4 * bb.area(),
              4 * bb.area()
            );

            rect_contrib > 0
          })
          .map(|bb| bb.area())
          .next()
          .ok_or_else(|| {
            AocError::Runtime("Found no bounding rectangles contained in the region".to_owned())
              .into()
          })
      }
    }
  }
}
