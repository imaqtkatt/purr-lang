use core::fmt;

use crate::{lexer::Token, typecheck};

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Token::Unexpected(c) => write!(f, "{c}"),
      Token::Identifier(ident) => write!(f, "{ident}"),
      Token::Generic(generic) => write!(f, "'{generic}"),
      Token::Number(num) => write!(f, "{num}"),
      Token::True => write!(f, "true"),
      Token::False => write!(f, "false"),
      Token::Unit => write!(f, "()"),
      Token::Type => write!(f, "type"),
      Token::Fun => write!(f, "fun"),
      Token::Plus => write!(f, "+"),
      Token::Minus => write!(f, "-"),
      Token::Asterisk => write!(f, "*"),
      Token::Slash => write!(f, "/"),
      Token::Dot => write!(f, "."),
      Token::Colon => write!(f, ":"),
      Token::Comma => write!(f, ","),
      Token::Semicolon => write!(f, ";"),
      Token::Arrow => write!(f, "->"),
      Token::LeftArrow => write!(f, "<-"),
      Token::LParens => write!(f, "("),
      Token::RParens => write!(f, ")"),
      Token::LBrace => write!(f, "{{"),
      Token::RBrace => write!(f, "}}"),
      Token::EOF => write!(f, "end of file"),
    }
  }
}

impl fmt::Display for typecheck::TypeKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      typecheck::TypeKind::Variable(name) => write!(f, "{name}"),
      typecheck::TypeKind::Generalized(g) => write!(f, "'{}", num_to_name(*g)),
      typecheck::TypeKind::Function(a, b) => {
        if let typecheck::TypeKind::Function(..) = &**a {
          write!(f, "({a}) -> {b}")
        } else {
          write!(f, "{a} -> {b}")
        }
      }
      typecheck::TypeKind::Hole(hole) => match hole.get() {
        typecheck::HoleKind::Bound(t) => write!(f, "{t}"),
        typecheck::HoleKind::Unbound(name, _) => write!(f, "{name}"),
      },
      typecheck::TypeKind::Error => write!(f, "?"),
    }
  }
}

fn num_to_name(mut num: usize) -> String {
  let mut s = String::new();
  loop {
    let c = (num % 26) as u8 + b'a';
    s.push(c as char);
    num /= 26;
    if num == 0 {
      break;
    }
  }
  s
}
