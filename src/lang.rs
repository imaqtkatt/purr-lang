use crate::{lexer::Lexer, parser::Parser, report::Reporter};

#[derive(Clone, Debug)]
pub enum Expression {
  Unit,
  Variable(String),
  Number(isize),
  Bool(bool),
  Block(Vec<Statement>),
  Function(Vec<Pattern>, Box<Expression>),
  Application(Box<Expression>, Vec<Expression>),
  BinaryOp(Operation, Box<Expression>, Box<Expression>),
  Branch(Box<Expression>, Box<Expression>, Box<Expression>),
  /*Match(Box<Expression>, TODO)*/
}

#[derive(Clone, Debug)]
pub enum Statement {
  Expression(Expression),
  Let(Pattern, Expression),
}

#[derive(Clone, Debug)]
pub enum Operation {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Shl,
  Shr,
  Eql,
  Neq,
  Gth,
  Lth,
  Gte,
  Lte,
}

#[derive(Clone, Debug)]
pub enum Pattern {
  Unit,
  Variable(String),
  Annot(String, Type),
}

#[derive(Clone, Debug)]
pub enum Type {
  Variable(String),
  Generic(String),
  Function(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug)]
pub enum TopLevel {
  Type(String, Type),
  Constant(String, Expression),
  Function(String, Vec<Pattern>, Expression),
}

#[derive(Debug)]
pub struct Program {
  pub file_name: String,
  pub definitions: Vec<TopLevel>,
}

impl Program {
  pub fn new(file_name: String, reporter: &Reporter) -> Option<Self> {
    if let Ok(source) = std::fs::read_to_string(&file_name) {
      let mut parser = Parser::new(Lexer::new(&source), reporter);
      let program = parser.program(file_name);
      program.ok()
    } else {
      None
    }
  }
}
