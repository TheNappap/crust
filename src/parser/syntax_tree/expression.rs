use crate::{parser::{Signature, Fn, Type}, lexer::{Literal, Span}};

use super::{patterns::Pattern, ordered_map::OrderedMap, fn_expr::Trait};

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpKind {
    Add, Sub, Mul, Div, Eq, Neq
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnOpKind {
    Neg
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression { kind, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    Fn(Fn),
    Signature(Signature),
    Impl(String, Vec<Fn>),
    Data(Type),
    Trait(Trait),
    Call(Signature, Vec<Expression>),
    New(Type, Vec<Expression>),
    Field(Box<Expression>, String, Type, i32),
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
    Case(Pattern, Vec<Expression>),
    Match(Box<Expression>, Type, OrderedMap<Pattern, Vec<Expression>>),
}