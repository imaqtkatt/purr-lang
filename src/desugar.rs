use crate::lang;

#[derive(Clone, Debug)]
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

impl FromIterator<Statement> for Expression {
  fn from_iter<T: IntoIterator<Item = Statement>>(iter: T) -> Self {
    Self::Block(iter.into_iter().collect())
  }
}

impl From<lang::Expression> for Expression {
  fn from(value: lang::Expression) -> Self {
    match value {
      lang::Expression::Unit => Self::Unit,
      lang::Expression::Variable(ident) => Self::Variable(ident),
      lang::Expression::Number(num) => Self::Number(num),
      lang::Expression::Bool(bool) => Self::Bool(bool),
      lang::Expression::Block(stmts) => {
        stmts.into_iter().map(Statement::from).collect::<Self>()
      }
      lang::Expression::Function(pats, body) => pats
        .into_iter()
        .rfold(Expression::from(*body), |body, pat| {
          Self::Function(pat.into(), Box::new(body))
        }),
      lang::Expression::Application(fun, args) => {
        args.into_iter().fold(Expression::from(*fun), |fun, arg| {
          Self::Application(Box::new(fun), Box::new(Expression::from(arg)))
        })
      }
      lang::Expression::BinaryOp(op, left, right) => {
        let left = Box::new(Expression::from(*left));
        let right = Box::new(Expression::from(*right));
        Self::BinaryOp(From::from(op), left, right)
      }
      lang::Expression::Branch(_, _, _) => todo!(),
    }
  }
}

impl From<lang::Statement> for Statement {
  fn from(value: lang::Statement) -> Self {
    match value {
      lang::Statement::Expression(e) => Self::Expression(Expression::from(e)),
      lang::Statement::Let(pat, value) => {
        Self::Let(Pattern::from(pat), Expression::from(value))
      }
    }
  }
}

impl From<lang::Operation> for Operation {
  fn from(value: lang::Operation) -> Self {
    match value {
      lang::Operation::Add => Self::Add,
      lang::Operation::Sub => Self::Sub,
      lang::Operation::Mul => Self::Mul,
      lang::Operation::Div => Self::Div,
      lang::Operation::Mod => Self::Mod,
      lang::Operation::Shl => Self::Shl,
      lang::Operation::Shr => Self::Shr,
      lang::Operation::Eql => Self::Eql,
      lang::Operation::Neq => Self::Neq,
      lang::Operation::Gth => Self::Gth,
      lang::Operation::Lth => Self::Lth,
      lang::Operation::Gte => Self::Gte,
      lang::Operation::Lte => Self::Lte,
    }
  }
}

impl From<lang::Pattern> for Pattern {
  fn from(value: lang::Pattern) -> Self {
    match value {
      lang::Pattern::Unit => Self::Unit,
      lang::Pattern::Variable(ident) => Self::Variable(ident),
      lang::Pattern::Annot(ident, t) => Self::Annot(ident, Type::from(t)),
    }
  }
}

impl From<lang::Type> for Type {
  fn from(value: lang::Type) -> Self {
    match value {
      lang::Type::Variable(ident) => Self::Variable(ident),
      lang::Type::Generic(generic) => Self::Generic(generic),
      lang::Type::Function(left, right) => Self::Function(
        Box::new(Type::from(*left)),
        Box::new(Type::from(*right)),
      ),
    }
  }
}

impl From<lang::TopLevel> for TopLevel {
  fn from(value: lang::TopLevel) -> Self {
    match value {
      lang::TopLevel::Type(name, t) => Self::Type(name, Type::from(t)),
      lang::TopLevel::Constant(name, value) => {
        Self::Constant(name, Expression::from(value))
      }
      lang::TopLevel::Function(name, pats, body) => Self::Function(
        name,
        pats.into_iter().map(Pattern::from).collect(),
        Expression::from(body),
      ),
    }
  }
}

impl From<lang::Program> for Program {
  fn from(
    lang::Program {
      file_name,
      definitions,
    }: lang::Program,
  ) -> Self {
    Self {
      file_name,
      definitions: definitions.into_iter().map(From::from).collect(),
    }
  }
}
