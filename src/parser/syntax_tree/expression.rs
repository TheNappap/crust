
use crate::{lexer::{Literal, Span}, parser::{Fn, Signature, Type}};

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
    pub forward: bool,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Expression { kind, span, forward: false }
    }

    pub fn forward(mut self) -> Self {
        self.forward = true;
        self
    }

    pub fn return_if_forward(mut self) -> Self {
        if self.forward {
            self.forward = false;
            let span = self.span.clone();
            Expression::new(ExpressionKind::Return(Box::new(self)), span)
        } else {
            self
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TransformKind {
    Map,
    Filter,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IterTransform {
    pub kind: TransformKind,
    pub fun: Fn,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    Fn(Fn),
    Signature(Signature),
    Impl(String, Vec<Fn>, Option<String>),
    Data(Type),
    Trait(Trait),
    Call(Signature, Vec<Expression>),
    New(Type, Vec<Expression>),
    Field(Box<Expression>, Symbol, i32),
    Let(Symbol, Box<Expression>),
    Mut(Symbol, Option<(Symbol, i32)>, Box<Expression>),
    If(Box<Expression>, Vec<Expression>, Option<Vec<Expression>>),
    While(Box<Expression>, Vec<Expression>),
    Fold(Box<Expression>, Symbol, Option<(Box<Expression>, Symbol)>, Vec<Expression>),
    Iter(Box<Expression>, Vec<IterTransform>, u32),
    Range(i64, i64),
    Group(Vec<Expression>),
    Literal(Literal),
    AddrOf(Vec<Expression>),
    Symbol(Symbol),
    BinOp(BinOpKind, Box<Expression>, Box<Expression>, Type),
    UnOp(UnOpKind, Box<Expression>, Type),
    Return(Box<Expression>),
    Array(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>, Type, u32),
    Case(Pattern, Vec<Expression>),
    Match(Box<Expression>, Type, OrderedMap<Pattern, Vec<Expression>>),
}