use crate::desugar;

#[derive(Clone, Debug, Default)]
pub enum Expression {
  Unit,
  Variable(String),
  Number(isize),
  Bool(bool),
  Block(Vec<Statement>),
  Function(Pattern, Box<Expression>),
  Application(Box<Expression>, Box<Expression>),
  BinaryOp(Operation, Box<Expression>, Box<Expression>),
  Branch(Box<Expression>, Box<Expression>, Box<Expression>),
  #[default]
  Error,
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

impl From<desugar::Type> for Type {
  fn from(value: desugar::Type) -> Self {
    match value {
      desugar::Type::Variable(name) => Self::Variable(name),
      desugar::Type::Generic(generic) => Self::Generic(generic),
      desugar::Type::Function(a, b) => {
        Self::Function(Box::new(Self::from(*a)), Box::new(Self::from(*b)))
      }
    }
  }
}
