

use core::slice::{Iter, IterMut};

pub mod fn_expr;
pub mod types;
pub mod expression;
pub mod data;

pub use expression::{Expression, BinOpKind, UnOpKind};

use self::data::Data;
use self::fn_expr::Fn;

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxTree {
    fns: Vec<Fn>,
    data: Vec<Data>,
}

impl SyntaxTree {
    pub fn new(fns: Vec<Fn>, data: Vec<Data>) -> SyntaxTree {
        SyntaxTree { fns, data }
    }

    pub fn fns(&self) -> Iter<Fn> {
        self.fns.iter()
    }

    pub fn fns_mut(&mut self) -> IterMut<Fn> {
        self.fns.iter_mut()
    }

    pub fn data(&self) -> Iter<Data> {
        self.data.iter()
    }
}
