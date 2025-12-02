use std::iter::successors;

use num_traits::PrimInt;

pub fn gcd<T>(a: T, b: T) -> T
where
  T: PrimInt,
{
  if a < b {
    gcd(b, a)
  } else if b == T::zero() {
    a
  } else {
    gcd(b, a % b)
  }
}

/// Returns the mathematical modulus n % modulus (i.e. -2 % 10 == 8).
pub fn modulus<T>(n: T, modulus: T) -> T
where
  T: PrimInt,
{
  ((n % modulus) + modulus) % modulus
}

pub fn digit_count<T>(num: T) -> u32
where
  T: PrimInt,
{
  let ten = T::from(10).unwrap();
  successors(Some(num), |&n| (n >= ten).then_some(n / ten)).count() as u32
}

#[cfg(test)]
mod tests {
  use crate::math::gcd;

  #[test]
  fn test_gcd_i32() {
    assert_eq!(gcd(12i32, 8i32), 4);
  }

  #[test]
  fn test_gcd_u32() {
    assert_eq!(gcd(31u32, 50u32), 1);
  }
}
