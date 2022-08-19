use crate::lexer::Value;

use self::fn_expr::Fn;
use core::slice::{Iter, IterMut};

pub mod fn_expr;


#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    Inferred
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(String, Vec<Expression>, Vec<Type>),
    Fn(Fn),
    Let(String, Box<Expression>),
    Literal(Literal),
    Pointer(Literal),
    Symbol(String, Type),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxTree {
    fns: Vec<Fn>,
}

impl SyntaxTree {
    pub fn new(fns: Vec<Fn>) -> SyntaxTree {
        SyntaxTree { fns }
    }

    pub fn fns(&self) -> Iter<Fn> {
        self.fns.iter()
    }

    pub fn fns_mut(&mut self) -> IterMut<Fn> {
        self.fns.iter_mut()
    }
}
