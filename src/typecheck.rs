use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
  desugar, elaborated as elab,
  report::{self, PurrReport},
};

pub struct TypeError(String);

impl PurrReport for TypeError {
  fn message(&self) -> String {
    let Self(msg) = self;
    msg.clone()
  }

  fn reason(&self) -> Vec<String> {
    vec![]
  }

  fn severity(&self) -> report::Severity {
    report::Severity::Error
  }
}

#[derive(Clone, Debug)]
pub enum TypeKind {
  Variable(String),
  Generalized(usize),
  Function(Type, Type),
  Hole(Hole),
  Error,
}

type Type = Rc<TypeKind>;

impl TypeKind {
  pub(crate) fn unit() -> Type {
    Type::new(TypeKind::Variable(String::from("unit")))
  }

  pub(crate) fn number() -> Type {
    Type::new(TypeKind::Variable(String::from("int")))
  }

  pub(crate) fn bool() -> Type {
    Type::new(TypeKind::Variable(String::from("bool")))
  }

  pub(crate) fn error() -> Type {
    Type::new(TypeKind::Error)
  }
}

#[derive(Clone, Debug)]
pub struct Scheme {
  pub binds: Vec<String>,
  pub t: Type,
}

#[derive(Debug, Clone)]
pub enum HoleKind {
  Bound(Type),
  Unbound(String, usize),
}

#[derive(Clone, Debug)]
pub struct Hole(Rc<RefCell<HoleKind>>);

impl PartialEq for Hole {
  fn eq(&self, other: &Self) -> bool {
    Rc::ptr_eq(&self.0, &other.0)
  }
}

impl Eq for Hole {}

impl Hole {
  pub fn new(name: String, level: usize) -> Self {
    Self(Rc::new(RefCell::new(HoleKind::Unbound(name, level))))
  }

  pub fn get(&self) -> HoleKind {
    self.0.borrow().clone()
  }

  pub fn get_mut(&mut self) -> std::cell::RefMut<'_, HoleKind> {
    self.0.borrow_mut()
  }

  pub fn fill(&self, t: Type) {
    *self.0.borrow_mut() = HoleKind::Bound(t)
  }
}

impl TypeKind {
  pub fn instantiate(self: Type, substitutions: &[Type]) -> Type {
    match &&*self {
      TypeKind::Variable(..) => self.clone(),
      TypeKind::Generalized(level) => substitutions[*level].clone(),
      TypeKind::Function(a, b) => {
        let a = a.clone().instantiate(substitutions);
        let b = b.clone().instantiate(substitutions);
        Rc::new(TypeKind::Function(a, b))
      }
      TypeKind::Hole(hole) => match hole.get() {
        HoleKind::Bound(t) => t.instantiate(substitutions),
        HoleKind::Unbound(..) => self.clone(),
      },
      TypeKind::Error => self.clone(),
    }
  }
}

impl Scheme {
  pub fn new(binds: Vec<String>, t: Type) -> Self {
    Self { binds, t }
  }
}

#[derive(Clone)]
pub struct Env {
  pub variables: HashMap<String, Scheme>,
  pub let_decls: HashMap<String, Type>,
  pub level: RefCell<usize>,
  pub counter: Rc<RefCell<usize>>,
  pub reporter: report::Reporter,
}

impl Env {
  pub fn new(reporter: report::Reporter) -> Self {
    Self {
      variables: Default::default(),
      let_decls: Default::default(),
      level: Default::default(),
      counter: Default::default(),
      reporter,
    }
  }

  fn from_ast(&self, t: desugar::Type) -> Type {
    match t {
      desugar::Type::Variable(name) => Type::new(TypeKind::Variable(name)),
      desugar::Type::Generic(generic) => {
        let level = *self.level.borrow();
        Type::new(TypeKind::Hole(Hole::new(generic, level)))
      }
      desugar::Type::Function(left, right) => {
        let left = self.from_ast(*left);
        let right = self.from_ast(*right);
        Type::new(TypeKind::Function(left, right))
      }
    }
  }

  fn lookup(&self, name: &str) -> Option<&Scheme> {
    self.variables.get(name)
  }

  fn instantiate(&self, Scheme { binds, t }: Scheme) -> Type {
    let substitutions = binds
      .into_iter()
      .map(|_| self.new_hole())
      .collect::<Vec<_>>();

    t.instantiate(&substitutions)
  }

  fn generalize(&self, t: Type) -> Scheme {
    let mut counter = 0;
    let level = *self.level.borrow();

    fn go(env_level: usize, t: Type, counter: &mut usize) {
      match &&*t {
        TypeKind::Hole(hole) => match hole.get() {
          HoleKind::Bound(t) => go(env_level, t, counter),
          HoleKind::Unbound(_, level) => {
            if level > env_level {
              let gen_level = *counter;
              *counter += 1;
              hole.fill(Type::new(TypeKind::Generalized(gen_level)));
            }
          }
        },
        TypeKind::Function(left, right) => {
          go(env_level, left.clone(), counter);
          go(env_level, right.clone(), counter);
        }
        TypeKind::Variable(..)
        | TypeKind::Generalized(..)
        | TypeKind::Error => (),
      }
    }

    go(level, t.clone(), &mut counter);
    let binds = (0..counter).map(|_| self.new_name()).collect::<Vec<_>>();

    Scheme { binds, t }
  }

  fn new_hole(&self) -> Type {
    let level = *self.level.borrow();
    Type::new(TypeKind::Hole(Hole::new(self.new_name(), level)))
  }

  fn new_name(&self) -> String {
    let mut counter = self.counter.borrow_mut();
    let name = format!("t_{}", *counter);
    *counter += 1;
    name
  }

  fn add_variable(&mut self, name: String, scheme: Scheme) {
    self.variables.insert(name, scheme);
  }

  fn enter_level(&self) {
    *self.level.borrow_mut() += 1;
  }

  fn exit_level(&self) {
    *self.level.borrow_mut() -= 1;
  }

  fn extend(&mut self, binds: HashMap<String, Type>) {
    for (name, t) in binds {
      let polytype = Scheme::new(vec![], t);
      self.add_variable(name, polytype);
    }
  }
}

pub(crate) trait Infer {
  type Out;

  fn infer(self, env: Env) -> (Self::Out, Type);
}

pub(crate) trait Declare {
  fn declare(self, env: &mut Env);
}

pub(crate) trait Define {
  fn define(self, env: &mut Env);
}

impl Infer for desugar::Expression {
  type Out = elab::Expression;

  fn infer(self, env: Env) -> (Self::Out, Type) {
    match self {
      desugar::Expression::Unit => (elab::Expression::Unit, TypeKind::unit()),
      desugar::Expression::Variable(name) => match env.lookup(&name) {
        Some(t) => {
          let tau = env.instantiate(t.clone());
          (elab::Expression::Variable(name), tau)
        }
        None => {
          let err = TypeError(format!("Unbound variable '{name}'"));
          env.reporter.report(err);
          (elab::Expression::Error, TypeKind::error())
        }
      },
      desugar::Expression::Number(num) => {
        (elab::Expression::Number(num), TypeKind::number())
      }
      desugar::Expression::Bool(b) => {
        (elab::Expression::Bool(b), TypeKind::bool())
      }
      desugar::Expression::Block(stmts) => {
        let mut return_type = TypeKind::unit();

        let mut env = env;
        let mut elab_stmts = Vec::new();
        for stmt in stmts {
          let ((elab_stmt, elab_env), t) = stmt.infer(env.clone());
          return_type = t;
          env = elab_env;
          elab_stmts.push(elab_stmt)
        }

        (elab::Expression::Block(elab_stmts), return_type)
      }
      desugar::Expression::Function(pat, body) => {
        // let hole = env.new_hole();
        let ((elab_pat, pat_env), t) = pat.infer(env.clone());
        // let scheme = Scheme {
        //   binds: vec![],
        //   t: t.clone(),
        // };

        let mut env_clone = env.clone();
        env_clone.extend(pat_env);
        // match pat {
        //   desugar::Pattern::Unit => (),
        //   desugar::Pattern::Variable(name)
        //   | desugar::Pattern::Annot(name, _) => {
        //     env_clone.add_variable(name, scheme);
        //   }
        // }

        let (elab_body, body_t) = body.infer(env_clone);
        let function_type = Type::new(TypeKind::Function(t, body_t));

        (
          elab::Expression::Function(elab_pat, Box::new(elab_body)),
          function_type,
        )
      }
      desugar::Expression::Application(fun, arg) => {
        let (elab_fun, fun_t) = fun.infer(env.clone());
        let (elab_arg, arg_t) = arg.infer(env.clone());

        let hole = env.new_hole();

        let function_type = Type::new(TypeKind::Function(arg_t, hole.clone()));
        if let Err(e) = unify(fun_t, function_type) {
          let err = TypeError(format!("Application error: {e}"));
          env.reporter.report(err);
          return (elab::Expression::Error, Type::new(TypeKind::Error));
        }
        (
          elab::Expression::Application(Box::new(elab_fun), Box::new(elab_arg)),
          hole,
        )
      }
      desugar::Expression::BinaryOp(_, _, _) => todo!(),
      desugar::Expression::Branch(_, _, _) => todo!(),
    }
  }
}

impl Infer for desugar::Statement {
  type Out = (elab::Statement, Env);

  fn infer(self, env: Env) -> (Self::Out, Type) {
    match self {
      desugar::Statement::Expression(e) => {
        let (elab_expr, t) = e.infer(env.clone());
        ((elab::Statement::Expression(elab_expr), env), t)
      }
      desugar::Statement::Let(bind, e) => {
        env.enter_level();
        let (elab_expr, t) = e.infer(env.clone());
        env.exit_level();

        let ((elab_pat, pat_env), bind_t) = bind.infer(env.clone());

        let g = env.generalize(t.clone());

        let mut env_clone = env.clone();
        env_clone.extend(pat_env);
        // env_clone.add_variable(bind.clone(), g);
        match &elab_pat {
          elab::Pattern::Unit => (),
          elab::Pattern::Variable(name) | elab::Pattern::Annot(name, _) => {
            env_clone.add_variable(name.clone(), g);
          }
        }

        if let Err(e) = unify(bind_t, t.clone()) {
          let err = TypeError(format!("Let error: {e}"));
          env.reporter.report(err);
          return (
            (
              elab::Statement::Expression(elab::Expression::Error),
              env_clone,
            ),
            TypeKind::error(),
          );
        }

        (
          (elab::Statement::Let(elab_pat, elab_expr), env_clone),
          TypeKind::unit(),
        )
      }
    }
  }
}

impl Infer for desugar::Pattern {
  type Out = (elab::Pattern, HashMap<String, Type>);

  fn infer(self, env: Env) -> (Self::Out, Type) {
    let mut map = HashMap::new();

    match self {
      desugar::Pattern::Unit => ((elab::Pattern::Unit, map), TypeKind::unit()),
      desugar::Pattern::Variable(name) => {
        let hole = env.new_hole();
        map.insert(name.clone(), hole.clone());
        ((elab::Pattern::Variable(name), map), hole)
      }
      desugar::Pattern::Annot(name, t) => {
        let elab_t = elab::Type::from(t.clone());
        let t = env.from_ast(t);
        map.insert(name.clone(), t.clone());
        ((elab::Pattern::Annot(name, elab_t), map), t)
      }
    }
  }
}

pub(crate) fn unify(t1: Type, t2: Type) -> Result<(), String> {
  use TypeKind::*;

  match (&*t1, &*t2) {
    (Variable(a), Variable(b)) if a == b => Ok(()),

    (Generalized(a), Generalized(b)) if a == b => Ok(()),

    (Hole(a), Hole(b)) if a == b => Ok(()),

    (Hole(a), _) => unify_hole(a.clone(), t2, false),
    (_, Hole(b)) => unify_hole(b.clone(), t1, true),

    (Function(a, b), Function(c, d)) => {
      unify(a.clone(), c.clone())?;
      unify(b.clone(), d.clone())
    }

    _ => Err(format!("Type mismatch between {t1} and {t2}.")),
  }
}

pub(crate) fn unify_hole(
  hole: Hole,
  t: Type,
  flip: bool,
) -> Result<(), String> {
  match hole.get() {
    HoleKind::Bound(x) if flip => unify(t, x),
    HoleKind::Bound(x) => unify(x, t),
    HoleKind::Unbound(..) => {
      if occurs(hole.clone(), t.clone()) {
        Err(format!("Occurs check"))
      } else {
        hole.fill(t);
        Ok(())
      }
    }
  }
}

pub(crate) fn occurs(hole: Hole, t: Type) -> bool {
  match &*t {
    TypeKind::Hole(x) => x.clone() == hole,
    TypeKind::Function(a, b) => {
      occurs(hole.clone(), a.clone()) || occurs(hole, b.clone())
    }
    TypeKind::Variable(..) | TypeKind::Generalized(..) | TypeKind::Error => {
      false
    }
  }
}
