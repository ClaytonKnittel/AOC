use std::{
    borrow::Borrow,
    fmt::Display,
    ops::{Add, AddAssign, Index, IndexMut, Mul, Sub},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pos {
    pub row: isize,
    pub col: isize,
}

impl Pos {
    pub fn zero() -> Self {
        Pos { row: 0, col: 0 }
    }
}

impl Add<Diff> for Pos {
    type Output = Self;

    fn add(self, rhs: Diff) -> Self::Output {
        Self {
            row: self.row + rhs.dr,
            col: self.col + rhs.dc,
        }
    }
}

impl AddAssign<Diff> for Pos {
    fn add_assign(&mut self, rhs: Diff) {
        *self = *self + rhs
    }
}

impl Sub for Pos {
    type Output = Diff;

    fn sub(self, rhs: Self) -> Self::Output {
        Diff {
            dr: self.row - rhs.row,
            dc: self.col - rhs.col,
        }
    }
}

impl Sub<Diff> for Pos {
    type Output = Pos;

    fn sub(self, rhs: Diff) -> Self::Output {
        Pos {
            row: self.row - rhs.dr,
            col: self.col - rhs.dc,
        }
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.row, self.col)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Diff {
    pub dr: isize,
    pub dc: isize,
}

impl Add for Diff {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            dr: self.dr + rhs.dr,
            dc: self.dc + rhs.dc,
        }
    }
}

impl Mul<Diff> for isize {
    type Output = Diff;

    fn mul(self, rhs: Diff) -> Self::Output {
        Diff {
            dr: self * rhs.dr,
            dc: self * rhs.dc,
        }
    }
}

impl Display for Diff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.dr, self.dc)
    }
}

#[derive(Clone)]
pub struct Grid {
    grid: Vec<Vec<u8>>,
}

impl Grid {
    pub fn new(grid: Vec<Vec<u8>>) -> Self {
        Self { grid }
    }

    pub fn width(&self) -> usize {
        self.grid.first().map(|row| row.len()).unwrap_or(0)
    }

    pub fn height(&self) -> usize {
        self.grid.len()
    }

    pub fn data(&self) -> &Vec<Vec<u8>> {
        &self.grid
    }

    pub fn in_bounds(&self, pos: Pos) -> bool {
        (0..self.width() as isize).contains(&pos.col)
            && (0..self.height() as isize).contains(&pos.row)
    }

    pub fn find_and_replace(&mut self, target: u8, replace: u8) -> Option<Pos> {
        self.grid.iter_mut().enumerate().find_map(|(row_idx, row)| {
            row.iter_mut().enumerate().find_map(|(col_idx, element)| {
                (*element == target).then(|| {
                    *element = replace;
                    Pos {
                        row: row_idx as isize,
                        col: col_idx as isize,
                    }
                })
            })
        })
    }

    pub fn neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> + '_ {
        (-1..=1).flat_map(move |dr| {
            (-1..=1)
                .filter_map(move |dc| (dr != 0 || dc != 0).then_some(pos + Diff { dr, dc }))
                .filter(|&pos| self.in_bounds(pos))
        })
    }

    pub fn orthogonal_neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> {
        [
            (pos.row != 0).then_some(Pos {
                row: pos.row.wrapping_sub(1),
                ..pos
            }),
            (pos.col != self.width() as isize - 1).then_some(Pos {
                col: pos.col + 1,
                ..pos
            }),
            (pos.row != self.height() as isize - 1).then_some(Pos {
                row: pos.row + 1,
                ..pos
            }),
            (pos.col != 0).then_some(Pos {
                col: pos.col.wrapping_sub(1),
                ..pos
            }),
        ]
        .into_iter()
        .flatten()
    }

    pub fn top_left_orthogonal_neighbors(&self, pos: Pos) -> impl Iterator<Item = Pos> {
        [
            (pos.row != 0).then_some(Pos {
                row: pos.row.wrapping_sub(1),
                ..pos
            }),
            (pos.col != 0).then_some(Pos {
                col: pos.col.wrapping_sub(1),
                ..pos
            }),
        ]
        .into_iter()
        .flatten()
    }

    pub fn positions(&self) -> impl Iterator<Item = Pos> + '_ {
        (0..self.height()).flat_map(|row| {
            (0..self.width()).map(move |col| Pos {
                row: row as isize,
                col: col as isize,
            })
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (Pos, u8)> + '_ {
        self.grid.iter().enumerate().flat_map(|(row_idx, row)| {
            row.iter().enumerate().map(move |(col_idx, &tile)| {
                (
                    Pos {
                        row: row_idx as isize,
                        col: col_idx as isize,
                    },
                    tile,
                )
            })
        })
    }
}

impl<P> Index<P> for Grid
where
    P: Borrow<Pos>,
{
    type Output = u8;

    fn index(&self, index: P) -> &Self::Output {
        &self.grid[index.borrow().row as usize][index.borrow().col as usize]
    }
}

impl<P> IndexMut<P> for Grid
where
    P: Borrow<Pos>,
{
    fn index_mut(&mut self, index: P) -> &mut Self::Output {
        &mut self.grid[index.borrow().row as usize][index.borrow().col as usize]
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.grid.iter().try_fold((), |_, row| {
            row.iter()
                .try_fold((), |_, &tile| write!(f, "{}", tile as char))
                .and_then(|_| writeln!(f))
        })
    }
}
