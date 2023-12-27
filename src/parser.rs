use crate::{
  lang::{
    self, Expression as E, Operation, Pattern, Statement, TopLevel as TL, Type,
  },
  lexer::{Lexer, Token},
  loc::Loc,
  report::{PurrReport, Reporter},
};

const PREC_TABLE: &[&[Token]] = &[
  &[Token::Plus, Token::Minus],
  &[Token::Asterisk, Token::Slash],
];

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'parser> {
  lexer: Lexer<'parser>,
  reporter: &'parser Reporter,
  curr: Loc<Token>,
  next: Loc<Token>,
}

#[derive(Clone)]
pub enum ParseError {
  UnexpectedToken { expected: Token, got: Loc<Token> },
  ExpectedIdentifier { got: Loc<Token> },
  ExpectedExpression { got: Loc<Token> },
}

impl<'parser> Parser<'parser> {
  pub fn new(mut lexer: Lexer<'parser>, reporter: &'parser Reporter) -> Self {
    let curr = lexer.next_token_located();
    let next = lexer.next_token_located();

    Self {
      lexer,
      reporter,
      curr,
      next,
    }
  }

  fn advance(&mut self) -> Loc<Token> {
    let mut ret = self.lexer.next_token_located();
    std::mem::swap(&mut self.next, &mut self.curr);
    std::mem::swap(&mut self.next, &mut ret);
    ret
  }

  fn is(&self, other: Token) -> bool {
    self.curr.val == other
  }

  fn expect(&mut self, expected: Token) -> ParseResult<Loc<Token>> {
    if self.curr.val == expected {
      Ok(self.advance())
    } else {
      let err = ParseError::UnexpectedToken {
        expected,
        got: self.curr.clone(),
      };
      self.reporter.report(err.clone());
      Err(err)
    }
  }

  fn expect_identifier(&mut self) -> ParseResult<String> {
    if let Token::Identifier(ident) = self.curr.val.clone() {
      self.advance();
      Ok(ident)
    } else {
      Err(ParseError::ExpectedIdentifier {
        got: self.curr.clone(),
      })
    }
  }
}

impl<'parser> Parser<'parser> {
  // x | 42 | () | ({expr})
  fn primary(&mut self) -> ParseResult<E> {
    let primary = match &self.curr.val {
      Token::Identifier(ident) => E::Variable(ident.clone()),
      Token::Number(num) => E::Number(num.clone()),
      Token::Unit => E::Unit,
      Token::True => E::Bool(true),
      Token::False => E::Bool(false),
      Token::LParens => {
        self.advance();
        let expr = self.expression()?;
        self.expect(Token::RParens)?;
        return Ok(expr);
      }
      _ => {
        let err = ParseError::ExpectedExpression {
          got: self.curr.clone(),
        };
        self.reporter.report(err.clone());
        return Err(err);
      }
    };
    self.advance();
    Ok(primary)
  }

  // x({expression}*)
  fn call(&mut self) -> ParseResult<E> {
    let callee = self.primary()?;

    if self.is(Token::LParens) {
      self.advance();
      let mut args = Vec::new();
      while !self.is(Token::RParens) {
        args.push(self.expression()?);
        if self.is(Token::Comma) {
          self.advance();
        } else {
          break;
        }
      }

      self.expect(Token::RParens)?;
      Ok(E::Application(Box::new(callee), args))
    } else {
      Ok(callee)
    }
  }

  // 1 + x(y)
  fn infix(&mut self, precedence: usize) -> ParseResult<E> {
    if precedence > PREC_TABLE.len() - 1 {
      return self.call();
    }

    let mut left = self.infix(precedence + 1)?;

    while PREC_TABLE[precedence].iter().any(|a| self.curr.val == *a) {
      let operation = self.operation()?;
      let right = self.infix(precedence + 1)?;
      left = E::BinaryOp(operation, Box::new(left), Box::new(right));
    }

    Ok(left)
  }

  // + | - | * | /
  fn operation(&mut self) -> ParseResult<Operation> {
    let op = match self.curr.val {
      Token::Plus => Operation::Add,
      Token::Minus => Operation::Sub,
      Token::Asterisk => Operation::Mul,
      Token::Slash => Operation::Div,
      _ => todo!(),
    };
    self.advance();
    Ok(op)
  }

  // fun pat+ -> {expr}
  fn function(&mut self) -> ParseResult<E> {
    self.expect(Token::Fun)?;
    let mut pats = vec![self.pattern()?];
    while !self.is(Token::Arrow) {
      pats.push(self.pattern()?);
    }
    self.advance();
    let expr = self.expression()?;
    Ok(E::Function(pats, Box::new(expr)))
  }

  // '{' {expr}+ | x <- {expr} ; '}'
  fn block(&mut self) -> ParseResult<E> {
    self.expect(Token::LBrace)?;
    let mut stmts = vec![self.stmt()?];
    while !self.is(Token::RBrace) {
      if self.is(Token::Semicolon) {
        self.advance();
      }
      if self.is(Token::Dot) {
        self.advance();
        break;
      }
      stmts.push(self.stmt()?);
    }
    self.expect(Token::RBrace)?;
    Ok(E::Block(stmts))
  }

  // fun | '{' stmt+ '}' | infix
  pub fn expression(&mut self) -> ParseResult<E> {
    match self.curr.val {
      Token::Fun => self.function(),
      Token::LBrace => self.block(),
      _ => self.infix(0),
    }
  }

  // () | x | x:{type}
  pub fn pattern(&mut self) -> ParseResult<Pattern> {
    let pat = match &self.curr.val {
      Token::Unit => Pattern::Unit,
      Token::Identifier(ident) if self.next.val == Token::Colon => {
        let ident = ident.clone();
        self.advance();
        self.advance();
        let typ = self.r#type()?;
        return Ok(Pattern::Annot(ident, typ));
      }
      Token::Identifier(ident) => Pattern::Variable(ident.clone()),
      t => todo!("{t:?}"),
    };
    self.advance();
    Ok(pat)
  }

  // ({type} -> {type})
  pub fn function_type(&mut self) -> ParseResult<Type> {
    let left = self.r#type()?;
    if self.is(Token::Arrow) {
      self.advance();
      let right = self.function_type()?;
      Ok(Type::Function(Box::new(left), Box::new(right)))
    } else {
      Ok(left)
    }
  }

  // x | 'a | {function_type}
  fn r#type(&mut self) -> ParseResult<Type> {
    let left = match &self.curr.val {
      Token::Identifier(ident) => Type::Variable(ident.clone()),
      Token::Generic(generic) => Type::Generic(generic.clone()),
      Token::LParens => {
        self.advance();
        let typ = self.function_type()?;
        self.expect(Token::RParens)?;
        return Ok(typ);
      }
      t => todo!("{t:?}"),
    };
    self.advance();
    Ok(left)
  }

  fn stmt(&mut self) -> ParseResult<Statement> {
    let stmt = match &self.next.val {
      Token::LeftArrow | Token::Colon => {
        let pat = self.pattern()?;
        self.expect(Token::LeftArrow)?;
        let expr = self.expression()?;
        Statement::Let(pat, expr)
      }
      _ => Statement::Expression(self.expression()?),
    };
    Ok(stmt)
  }

  // ident {pat}+ -> {expr} | ident -> {expr}
  pub fn function_definition(&mut self) -> ParseResult<TL> {
    let fun_name = self.expect_identifier()?;
    if self.is(Token::Arrow) {
      self.advance();
      let expr = self.expression()?;
      return Ok(TL::Constant(fun_name, expr));
    }
    let mut args = vec![self.pattern()?];
    while !self.is(Token::Arrow) {
      args.push(self.pattern()?);
    }
    self.expect(Token::Arrow)?;
    let body = self.expression()?;
    Ok(TL::Function(fun_name, args, body))
  }

  // 'type' ident : {function_type}
  pub fn type_definition(&mut self) -> ParseResult<TL> {
    self.expect(Token::Type)?;
    let type_name = self.expect_identifier()?;
    self.expect(Token::Colon)?;
    let typ = self.function_type()?;
    Ok(TL::Type(type_name, typ))
  }

  pub fn top_level(&mut self) -> ParseResult<TL> {
    match &self.curr.val {
      Token::Type => self.type_definition(),
      _ => self.function_definition(),
    }
  }

  pub fn program(&mut self, file_name: String) -> ParseResult<lang::Program> {
    let mut definitions = vec![];
    while let Ok(tl) = self.top_level() {
      definitions.push(tl);
    }
    Ok(lang::Program {
      definitions,
      file_name,
    })
  }
}

impl PurrReport for ParseError {
  fn message(&self) -> String {
    match self {
      ParseError::UnexpectedToken { expected, got } => {
        format!("Expected '{}' but got '{}'.", expected, got.val)
      }
      ParseError::ExpectedIdentifier { got } => {
        format!("Expected an identifier but got '{}'.", got.val)
      }
      ParseError::ExpectedExpression { got } => {
        format!("Expected an expression but got '{}'.", got.val)
      }
    }
  }

  fn reason(&self) -> Vec<String> {
    vec![]
  }

  fn severity(&self) -> crate::report::Severity {
    crate::report::Severity::Error
  }
}
