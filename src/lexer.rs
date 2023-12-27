use std::{iter::Peekable, str::Chars};

use crate::loc;
use crate::loc::Loc;

pub struct Lexer<'code> {
  source: Peekable<Chars<'code>>,
  pos: usize,
}

impl<'code> Lexer<'code> {
  pub fn new(source: &'code str) -> Self {
    let source = source.chars().peekable();
    let pos = 0;
    Self { source, pos }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
  Unexpected(char),
  Identifier(String),
  Generic(String),
  Number(isize),
  True,
  False,
  Unit,
  Type,
  Fun,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Dot,
  Colon,
  Comma,
  Semicolon,
  Arrow,
  LeftArrow,
  LParens,
  RParens,
  LBrace,
  RBrace,
  EOF,
}

impl<'code> Lexer<'code> {
  fn next_token(&mut self) -> Token {
    if let Some(c) = self.source.peek() {
      match c {
        '(' => self.read_lparens(),
        ')' => self.just(Token::RParens),
        '{' => self.just(Token::LBrace),
        '}' => self.just(Token::RBrace),
        '<' => self.read_lt(),
        '>' => self.read_gt(),
        '.' => self.just(Token::Dot),
        ',' => self.just(Token::Comma),
        ':' => self.just(Token::Colon),
        ';' => self.just(Token::Semicolon),
        '+' => self.just(Token::Plus),
        '*' => self.just(Token::Asterisk),
        '-' => self.read_minus(),
        '/' => self.read_slash(),
        '\'' => self.read_generic(),
        c if can_skip(c) => {
          self.consume();
          self.next_token()
        }
        i if i.is_ascii_alphabetic() => self.identifier(),
        n if n.is_digit(10) => self.number(),
        _ => self.unexpected(),
      }
    } else {
      Token::EOF
    }
  }

  fn just(&mut self, token: Token) -> Token {
    self.consume();
    token
  }

  fn unexpected(&mut self) -> Token {
    Token::Unexpected(self.consume().unwrap())
  }

  fn consume(&mut self) -> Option<char> {
    self.pos += 1;
    self.source.next()
  }

  fn read_gt(&mut self) -> Token {
    self.consume();
    match self.source.peek() {
      Some('>') => todo!(),
      _ => Token::RParens,
    }
  }

  fn read_lt(&mut self) -> Token {
    self.consume();
    match self.source.peek() {
      Some('<') => todo!(),
      Some('-') => self.just(Token::LeftArrow),
      _ => Token::LParens,
    }
  }

  fn read_lparens(&mut self) -> Token {
    self.consume();
    match self.source.peek() {
      Some(')') => self.just(Token::Unit),
      _ => Token::LParens,
    }
  }

  fn read_minus(&mut self) -> Token {
    self.consume();
    match self.source.peek() {
      Some('>') => self.just(Token::Arrow),
      _ => Token::Minus,
    }
  }

  fn read_slash(&mut self) -> Token {
    self.consume();
    match self.source.peek() {
      Some('/') => {
        self.ignore_until(|c| *c != '\n');
        self.next_token()
      }
      _ => Token::Slash,
    }
  }

  fn identifier(&mut self) -> Token {
    let ident = self.consume_while(|c| c.is_alphanumeric() || *c == '_');
    match ident.as_str() {
      "fun" => Token::Fun,
      "type" => Token::Type,
      "true" => Token::True,
      "false" => Token::False,
      _ => Token::Identifier(ident),
    }
  }

  fn number(&mut self) -> Token {
    let num = self.consume_while(|c| c.is_digit(10));
    Token::Number(num.parse().unwrap())
  }

  fn read_generic(&mut self) -> Token {
    self.consume();
    let ident = self.consume_while(|c| c.is_ascii_alphabetic());
    Token::Generic(ident)
  }

  fn consume_while(&mut self, f: fn(&char) -> bool) -> String {
    let mut s = String::new();
    while let Some(c) = self.source.peek() {
      if f(c) {
        s.push(self.consume().unwrap());
      } else {
        break;
      }
    }
    s
  }

  fn ignore_until(&mut self, f: fn(&char) -> bool) {
    while let Some(c) = self.source.peek() {
      if f(c) {
        self.consume().unwrap();
      } else {
        break;
      }
    }
  }

  pub fn next_token_located(&mut self) -> Loc<Token> {
    let start = self.pos;
    if let Some(c) = self.source.peek() {
      match c {
        s if can_skip(s) => {
          self.consume();
          self.next_token_located()
        }
        _ => {
          let start = self.pos;
          let token = self.next_token();
          let end = self.pos;
          loc!(token, start..end)
        }
      }
    } else {
      let end = self.pos;
      loc!(Token::EOF, start..end)
    }
  }
}

impl<'code> Iterator for Lexer<'code> {
  type Item = Loc<Token>;

  fn next(&mut self) -> Option<Self::Item> {
    let next = self.next_token_located();
    match *next {
      Token::EOF => None,
      _ => Some(next),
    }
  }
}

fn can_skip(c: &char) -> bool {
  matches!(c, ' ' | '\n' | '\t')
}
