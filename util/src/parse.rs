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

pub fn parse_delim<I>(line: &str, delim: &str) -> AocResult<Vec<I>>
where
    I: FromStr,
    <I as FromStr>::Err: std::error::Error + 'static,
{
    Ok(line
        .split(delim)
        .map(|num| num.parse::<I>())
        .collect::<Result<Vec<I>, _>>()?)
}

pub fn parse_list<I>(line: &str) -> AocResult<Vec<I>>
where
    I: FromStr,
    <I as FromStr>::Err: std::error::Error + 'static,
{
    Ok(line
        .split_whitespace()
        .map(|num| num.parse::<I>())
        .collect::<Result<Vec<I>, _>>()?)
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

pub fn list_of_objects<T>(path: &str) -> AocResult<Vec<T>>
where
    T: FromStr,
    Box<dyn std::error::Error>: From<<T as FromStr>::Err>,
{
    list_of_strings(path)?
        .map(|line| -> AocResult<_> { line?.parse().map_err(Box::from) })
        .collect::<Result<Vec<T>, _>>()
}

pub fn parse_grid(path: &str) -> AocResult<Grid> {
    let file = File::open(path)?;
    let len = file.metadata()?.len();
    let buf = io::BufReader::new(file);
    let mut grid = Vec::with_capacity(len as usize);

    let mut lines = buf.lines();
    let line1 = lines
        .next()
        .ok_or_else(|| AocError::Parse("Unexpected empty input!".to_owned()))??;
    let width = line1.len();
    grid.extend_from_slice(line1.as_bytes());
    let mut height = 1;
    for line in lines {
        grid.extend_from_slice(line?.as_bytes());
        height += 1;
    }

    Ok(Grid::new(grid, width, height))
}
