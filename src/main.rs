use report::Reporter;
use typecheck::Define;

use crate::typecheck::{Declare, Env};

pub mod desugar;
pub(crate) mod display;
pub mod elaborated;
pub mod lang;
pub mod lexer;
pub mod loc;
pub mod parser;
pub mod report;
pub mod typecheck;

fn main() {
  let (ref reporter, recv) = Reporter::new();

  let file_name = String::from("./main.purr");
  if let Some(program) = lang::Program::new(file_name, reporter) {
    let desugar = desugar::Program::from(program);
    let mut env = Env::new(reporter.clone());

    for def in desugar.definitions.clone() {
      if let desugar::TopLevel::Function(fun) = def {
        fun.define(&mut env);
      }
    }

    for def in desugar.definitions {
      if let desugar::TopLevel::Function(fun) = def {
        fun.declare(&mut env);
      }
    }
  }
  Reporter::to_stdout(recv)
}
