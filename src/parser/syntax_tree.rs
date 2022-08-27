
use crate::lexer::{Literal, Token};

use self::fn_expr::{Fn, Signature};
use core::slice::{Iter, IterMut};

pub mod fn_expr;

#[derive(Debug, PartialEq, Clone)]
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
                ty => todo!()
            }
            Token::Literal(_) => todo!(),
            Token::Symbol(_) => todo!(),
            Token::Group(_, _) => todo!(),
            Token::NewLine => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Call(Signature, Vec<Expression>),
    Fn(Fn),
    If(Box<Expression>, Vec<Expression>),
    Let(String, Box<Expression>, Type),
    Literal(Literal),
    AddrOf(Box<Expression>),
    Symbol(String, Type),
    Add(Box<Expression>,Box<Expression>, Type),
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
