use std::collections::HashMap;

use crate::desugar::{self, top_level};

struct NamingCtx(HashMap<String, Vec<usize>>);

impl NamingCtx {
  fn push(&mut self, name: String, count: &mut usize) {
    self.0.entry(name).or_default().push(*count);
    *count += 1;
  }

  fn pop(&mut self, name: &String) -> String {
    let gen = self.0.get_mut(name).unwrap().pop().unwrap();
    if self.0[name].is_empty() {
      self.0.remove(name);
    }
    format!("{name}_{gen}")
  }

  fn consume(&self, name: &String) -> String {
    if let Some(uses) = self.0.get(name) {
      let gen = *uses.last().unwrap();
      format!("{name}_{gen}")
    } else {
      name.clone()
    }
  }
}

impl desugar::Pattern {
  fn push_var(&self, ctx: &mut NamingCtx, count: &mut usize) {
    match self {
      desugar::Pattern::Unit => (),
      desugar::Pattern::Variable(name) | desugar::Pattern::Annot(name, _) => {
        ctx.push(name.clone(), count);
      }
    }
  }

  fn pop_var(&mut self, ctx: &mut NamingCtx) {
    match self {
      desugar::Pattern::Unit => (),
      desugar::Pattern::Variable(name) | desugar::Pattern::Annot(name, _) => {
        *name = ctx.pop(name);
      }
    }
  }

  fn consume(&mut self, ctx: &mut NamingCtx) {
    match self {
      desugar::Pattern::Unit => (),
      desugar::Pattern::Variable(name) | desugar::Pattern::Annot(name, _) => {
        *name = ctx.consume(name);
      }
    }
  }
}

impl desugar::Expression {
  fn gen_unique_names(&mut self, ctx: &mut NamingCtx, count: &mut usize) {
    match self {
      desugar::Expression::Variable(name) => {
        *name = ctx.consume(name);
      }
      desugar::Expression::Function(pat, body) => {
        pat.push_var(ctx, count);
        body.gen_unique_names(ctx, count);
        pat.pop_var(ctx);
      }
      desugar::Expression::Block(stmts) => {
        for stmt in stmts {
          stmt.gen_unique_names(ctx, count);
        }
      }
      desugar::Expression::Application(fun, arg) => {
        fun.gen_unique_names(ctx, count);
        arg.gen_unique_names(ctx, count);
      }
      desugar::Expression::BinaryOp(_, _, _) => todo!(),
      desugar::Expression::Branch(_, _, _) => todo!(),
      desugar::Expression::Unit
      | desugar::Expression::Number(..)
      | desugar::Expression::Bool(..) => (),
    }
  }
}

impl desugar::Statement {
  fn gen_unique_names(&mut self, ctx: &mut NamingCtx, count: &mut usize) {
    match self {
      desugar::Statement::Expression(e) => e.gen_unique_names(ctx, count),
      desugar::Statement::Let(pat, body) => {
        body.gen_unique_names(ctx, count);
        pat.push_var(ctx, count);
        pat.consume(ctx);
      }
    }
  }
}

impl desugar::Program {
  pub fn unique_names(&mut self) {
    let mut ctx = NamingCtx(HashMap::new());
    let count = &mut 0;

    for def in &mut self.definitions {
      match def {
        desugar::TopLevel::Type(..) => (),
        desugar::TopLevel::Constant(c) => {
          c.value.gen_unique_names(&mut ctx, count);
        }
        desugar::TopLevel::Function(fun) => {
          fun.gen_unique_names(&mut ctx, count);
        }
      }
    }
  }
}

impl top_level::Function {
  fn gen_unique_names(&mut self, ctx: &mut NamingCtx, count: &mut usize) {
    for pat in &self.pats {
      pat.push_var(ctx, count);
    }
    self.body.gen_unique_names(ctx, count);
    for pat in &mut self.pats {
      pat.pop_var(ctx);
    }
  }
}
