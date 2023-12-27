use core::fmt;
use std::ops::Range;

#[derive(Clone, Debug)]
pub struct Loc<T>
where
  T: Sized + fmt::Debug,
{
  pub val: T,
  pub span: Range<usize>,
}

impl<T: Sized + fmt::Debug> Loc<T> {
  pub fn new(val: T, span: Range<usize>) -> Self {
    Self { val, span }
  }
}

impl<T: fmt::Debug> std::ops::Deref for Loc<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.val
  }
}

#[macro_export]
macro_rules! loc {
  ($val:expr, $span:expr) => {
    Loc::new($val, $span)
  };
}
