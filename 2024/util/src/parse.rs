use std::{
  fs::File,
  io::{self, BufRead},
  str::FromStr,
};

use crate::{
  error::{AocError, AocResult},
  grid::Grid,
};

pub fn list_of_strings(path: &str) -> io::Result<impl Iterator<Item = io::Result<String>>> {
  Ok(io::BufReader::new(File::open(path)?).lines())
}

pub fn two_lists<I>(path: &str) -> AocResult<(Vec<I>, Vec<I>)>
where
  I: FromStr,
  <I as FromStr>::Err: std::error::Error + 'static,
{
  list_of_strings(path)?.try_fold(
    (Vec::new(), Vec::new()),
    |(mut v1, mut v2), line| -> AocResult<_> {
      let mut x = line?
        .split_whitespace()
        .map(|num| num.parse::<I>())
        .collect::<Result<Vec<I>, _>>()?;
      if !x.is_empty() {
        if x.len() != 2 {
          return Err(
            AocError::Parse(format!("Expected 2 integers in a line, found {}", x.len())).into(),
          );
        }
        v2.push(x.pop().unwrap());
        v1.push(x.pop().unwrap());
      }
      Ok((v1, v2))
    },
  )
}

pub fn parse_delim<I>(line: &str, delim: &str) -> AocResult<Vec<I>>
where
  I: FromStr,
  <I as FromStr>::Err: std::error::Error + 'static,
{
  Ok(
    line
      .split(delim)
      .map(|num| num.parse::<I>())
      .collect::<Result<Vec<I>, _>>()?,
  )
}

pub fn parse_list<I>(line: &str) -> AocResult<Vec<I>>
where
  I: FromStr,
  <I as FromStr>::Err: std::error::Error + 'static,
{
  Ok(
    line
      .split_whitespace()
      .map(|num| num.parse::<I>())
      .collect::<Result<Vec<I>, _>>()?,
  )
}

pub fn list_of_lists<I>(path: &str) -> AocResult<Vec<Vec<I>>>
where
  I: FromStr,
  <I as FromStr>::Err: std::error::Error + 'static,
{
  list_of_strings(path)?.try_fold(Vec::new(), |mut lists, line| -> AocResult<_> {
    lists.push(parse_list(&line?)?);
    Ok(lists)
  })
}

pub fn list_of_chars(path: &str) -> AocResult<Vec<Vec<char>>> {
  list_of_strings(path)?.try_fold(Vec::new(), |mut lists, line| -> AocResult<_> {
    lists.push(line?.chars().collect());
    Ok(lists)
  })
}

pub fn parse_grid(path: &str) -> AocResult<Grid> {
  Ok(Grid::new(
    list_of_chars(path)?
      .into_iter()
      .map(|row| row.into_iter().map(|c| c as u8).collect())
      .collect(),
  ))
}
