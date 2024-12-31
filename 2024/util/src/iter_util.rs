use crate::error::{AocError, AocResult};

pub fn iter_to_slice<const N: usize, I, T>(iter: I) -> AocResult<[T; N]>
where
  I: IntoIterator<Item = T>,
{
  fn helper<const N: usize, I, T>(
    mut slice: [Option<T>; N],
    mut iter: I,
    idx: usize,
  ) -> AocResult<[T; N]>
  where
    I: Iterator<Item = T>,
  {
    if idx == N {
      Ok(slice.map(|maybe_t| unsafe { maybe_t.unwrap_unchecked() }))
    } else {
      let t = iter.next().ok_or_else(|| {
        AocError::Runtime(format!(
          "Ran out of elements in iterator, expected {} more",
          N - idx
        ))
      })?;
      slice[idx] = Some(t);
      helper::<N, I, T>(slice, iter, idx + 1)
    }
  }

  helper([(); N].map(|_| None), iter.into_iter(), 0)
}
