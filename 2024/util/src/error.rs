use core::fmt;
use std::{
  error::Error,
  fmt::{Display, Formatter},
};

#[derive(Debug)]
pub enum AocError {
  Parse(String),
  Runtime(String),
}

impl Display for AocError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      AocError::Parse(msg) => write!(f, "Parse error: {msg}"),
      AocError::Runtime(msg) => write!(f, "Runtime error: {msg}"),
    }
  }
}

impl Error for AocError {}

pub type AocResult<T = ()> = Result<T, Box<dyn Error>>;
