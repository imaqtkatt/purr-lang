use lexer::Lexer;
use parser::Parser;
use report::Reporter;
use typecheck::Infer;

use crate::typecheck::Env;

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
  let src = std::fs::read_to_string(file_name).unwrap();
  let l = Lexer::new(&src);
  let mut pr = Parser::new(l, reporter);
  if let Ok(e) = pr.expression() {
    let e = desugar::Expression::from(e);
    let env = Env::new(reporter.clone());
    let (_elab_e, _t) = e.infer(env);
    // println!("{elab_e:?}");
    // println!("{t}");
  }

  Reporter::to_stdout(recv)
}
