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
