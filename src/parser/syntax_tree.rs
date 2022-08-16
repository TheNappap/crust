use crate::lexer::Value;

use self::fn_expr::Fn;
use core::slice::Iter;

pub mod fn_expr;


#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    String
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(String, Vec<Expression>, Vec<Expression>),
    Fn(Fn),
    Let(String, Value),
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
}
