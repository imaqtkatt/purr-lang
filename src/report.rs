use core::fmt;
use std::sync::mpsc;

#[derive(Clone, Debug)]
pub enum Severity {
  Warning,
  Info,
  Error,
}

pub trait PurrReport {
  fn message(&self) -> String;
  fn reason(&self) -> Vec<String>;
  fn severity(&self) -> Severity;
}

#[derive(Clone)]
pub struct Reporter(mpsc::Sender<Box<dyn PurrReport>>);

impl Reporter {
  pub fn new() -> (Self, mpsc::Receiver<Box<dyn PurrReport>>) {
    let (send, recv) = mpsc::channel();
    (Self(send), recv)
  }

  pub fn report(&self, report: impl PurrReport + 'static) {
    self.0.send(Box::new(report)).unwrap();
  }

  pub fn to_stdout(receiver: mpsc::Receiver<Box<dyn PurrReport>>) {
    for report in receiver.try_iter() {
      println!("{} - {}", report.severity(), report.message());
      for reason in report.reason() {
        println!("  > {}", reason)
      }
      println!()
    }
  }
}

impl fmt::Display for Severity {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Severity::Warning => write!(f, "[warning]"),
      Severity::Info => write!(f, "[info]"),
      Severity::Error => write!(f, "[error]"),
    }
  }
}
