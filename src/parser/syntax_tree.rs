

use core::slice::{Iter, IterMut};

pub mod fn_expr;
pub mod types;
pub mod expression;
pub mod data;
pub mod field_map;

pub use expression::{Expression, BinOpKind, UnOpKind};

use self::data::Data;
use self::fn_expr::Fn;

pub trait Library {
    fn fns(&self) -> Vec<Fn>;
    fn data(&self) -> Vec<Data>;
}

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

    pub fn add_lib(&mut self, lib: &dyn Library) {
        self.fns.extend(lib.fns());
        self.data.extend(lib.data());
    }
}
