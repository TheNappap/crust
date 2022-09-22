
use crate::lexer::{Literal, Token};

use self::fn_expr::{Fn, Signature};
use core::slice::{Iter, IterMut};

pub mod fn_expr;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Inferred
}

impl From<Token> for Type {
    fn from(token: Token) -> Self {
        match token {
            Token::Ident(ty) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                _ty => todo!()
            }
            Token::Literal(_) => todo!(),
            Token::Symbol(_) => todo!(),
            Token::Operator(_) => todo!(),
            Token::Group(_, _) => todo!(),
            Token::NewLine => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpKind {
    Add, Sub, Mul, Div, Eq, Neq
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(Signature, Vec<Expression>),
    Fn(Fn),
    Let(String, Box<Expression>, Type),
    Mut(String, Box<Expression>),
    If(Box<Expression>, Vec<Expression>, Option<Vec<Expression>>),
    While(Box<Expression>, Vec<Expression>),
    Literal(Literal),
    AddrOf(Vec<Expression>),
    Symbol(String, Type),
    BinOp(BinOpKind , Box<Expression>, Box<Expression>, Type),
    Return(Box<Expression>),
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
