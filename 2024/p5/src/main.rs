use std::{
  collections::{hash_map::Entry, HashMap, HashSet},
  fs::File,
  io::{BufRead, BufReader},
};

use util::{
  error::{AocError, AocResult},
  parse::parse_delim,
};

struct PageInfo {
  predecessors: Vec<u32>,
}

struct RuleGraph {
  page_info: HashMap<u32, PageInfo>,
}

impl RuleGraph {
  fn new() -> Self {
    Self {
      page_info: HashMap::new(),
    }
  }

  fn add_rule(&mut self, pred: u32, succ: u32) {
    match self.page_info.entry(succ) {
      Entry::Occupied(mut entry) => {
        entry.get_mut().predecessors.push(pred);
      }
      Entry::Vacant(entry) => {
        let mut predecessors = Vec::new();
        predecessors.push(pred);
        entry.insert(PageInfo { predecessors });
      }
    }
  }

  fn predecessors(&self, page: u32) -> Vec<u32> {
    self
      .page_info
      .get(&page)
      .map(|page_info| page_info.predecessors.clone())
      .unwrap_or(Vec::new())
  }

  fn is_valid_print(&self, print: &Vec<u32>) -> bool {
    let mut invalid_pages = HashSet::new();

    for &page in print {
      if invalid_pages.contains(&page) {
        return false;
      }

      for pred in self.predecessors(page) {
        invalid_pages.insert(pred);
      }
    }

    true
  }
}

fn parse_input(input_file: &str) -> AocResult<(RuleGraph, Vec<Vec<u32>>)> {
  let lines = BufReader::new(File::open(input_file)?).lines();
  let mut parsing_rules = true;
  let mut rule_graph = RuleGraph::new();
  let mut prints = Vec::new();

  for line in lines {
    let line = line?;
    if parsing_rules && line.is_empty() {
      parsing_rules = false;
      continue;
    }

    if parsing_rules {
      let rule: Vec<_> = line.split('|').collect();
      if rule.len() != 2 {
        return Err(AocError::Parse(format!("Line '{}' not formatted <int>|<int>", line)).into());
      }
      let pred = rule[0].parse::<u32>()?;
      let succ = rule[1].parse::<u32>()?;
      rule_graph.add_rule(pred, succ);
    } else {
      prints.push(parse_delim(&line, ",")?);
    }
  }

  Ok((rule_graph, prints))
}

fn main() -> AocResult {
  const INPUT_FILE: &str = "input.txt";
  let (rule_graph, prints) = parse_input(INPUT_FILE)?;

  let valid_prints = prints
    .iter()
    .flat_map(|print| {
      if rule_graph.is_valid_print(print) {
        Some(print[print.len() / 2])
      } else {
        None
      }
    })
    .sum::<u32>();

  println!("Valid prints: {valid_prints}");

  Ok(())
}
