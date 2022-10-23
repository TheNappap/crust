
use crate::lexer::{Literal};

use self::{fn_expr::{Fn, Signature}, types::Type};
use core::slice::{Iter, IterMut};

pub mod fn_expr;
pub mod types;

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpKind {
    Add, Sub, Mul, Div, Eq, Neq
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOpKind {
    Neg
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(Signature, Vec<Expression>),
    Fn(Fn),
    Let(String, Box<Expression>, Type),
    Mut(String, Box<Expression>),
    If(Box<Expression>, Vec<Expression>, Option<Vec<Expression>>),
    While(Box<Expression>, Vec<Expression>),
    Iter(Box<Expression>),
    Literal(Literal),
    AddrOf(Vec<Expression>),
    Symbol(String, Type),
    BinOp(BinOpKind, Box<Expression>, Box<Expression>, Type),
    UnOp(UnOpKind, Box<Expression>, Type),
    Return(Box<Expression>),
    Array(Vec<Expression>),
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
