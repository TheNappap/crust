use crate::{parser::{Signature, Fn, Type}, lexer::Literal};

use super::data::Data;

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
    Struct(Data),
    New(Data, Vec<Expression>),
    Let(String, Box<Expression>, Type),
    Mut(String, Box<Expression>),
    If(Box<Expression>, Vec<Expression>, Option<Vec<Expression>>),
    While(Box<Expression>, Vec<Expression>),
    For(Box<Expression>, String, Type, Vec<Expression>),
    Iter(Box<Expression>, u32),
    Group(Vec<Expression>),
    Literal(Literal),
    AddrOf(Vec<Expression>),
    Symbol(String, Type),
    BinOp(BinOpKind, Box<Expression>, Box<Expression>, Type),
    UnOp(UnOpKind, Box<Expression>, Type),
    Return(Box<Expression>),
    Array(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>, Type, u32),
}