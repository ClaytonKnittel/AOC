use std::{
    fs::File,
    io::{self, BufRead},
    str::FromStr,
};

use crate::error::{AocError, AocResult};

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
            if x.len() != 0 {
                if x.len() != 2 {
                    return Err(AocError::Parse(format!(
                        "Expected 2 integers in a line, found {}",
                        x.len()
                    ))
                    .into());
                }
                v2.push(x.pop().unwrap());
                v1.push(x.pop().unwrap());
            }
            Ok((v1, v2))
        },
    )
}
