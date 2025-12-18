
use std::collections::HashMap;

use crate::{lexer::{Token, TokenKind, Delimeter}, utils::{Result, ThrowablePosition}};

use super::ordered_map::OrderedMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Never,
    Inferred,
    Named(String),
    Array(Box<Type>, usize),
    Range(u32),
    Struct(String, OrderedMap<String, (Type, i32)>),
    Enum(String, HashMap<String, usize>),
    Iter(Box<Type>),
}

impl Type {
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            Int | Float | Bool | String | Range(_) | Iter(_) => 8,
            Type::Array(ty, len) => ty.size()*(*len as u32),
            Type::Struct(_, types) => types.values().map(|(t,_)| t.size()).sum(),
            Type::Enum(_, _) => Int.size(),
            Void => 0,
            Never | Inferred | Named(_) => unreachable!(),
        }
    }

    pub fn name(&self) -> String {
        match self {
            Type::Int => "Int".into(),
            Type::Float => "Float".into(),
            Type::Bool => "Bool".into(),
            Type::String => "String".into(),
            Type::Range(_) => "Range".into(),
            Type::Void => "Void".into(),
            Type::Never => "Never".into(),
            Type::Inferred => "Inferred".into(),
            Type::Named(name) => name.into(),
            Type::Array(ty, size) => format!("[{}; {size}]", ty.name()),
            Type::Struct(name, _) => name.into(),
            Type::Enum(name, _) => name.into(),
            Type::Iter(ty) => format!("Iter[{}]", ty.name()),
        }
    }

    pub fn from(token: Token) -> Result<Self> {
        use crate::lexer::Literal::*;
        use TokenKind::*;
        let ty = match token.kind {
            Ident(ty) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                name => Type::Named(name.to_owned())
            }
            Group(Delimeter::Brackets, tokens) => {
                match tokens.as_slice() {
                    [token @ Token{kind: Ident(_), ..}, Token{kind: Semicolon, ..}, Token{kind: Literal(Int(n)), ..}] =>
                        Type::Array(Box::new(Type::from(token.clone())?), *n as usize),
                    _ => return token.span.syntax("Unknown type".into())
                }
            }
            kind => unreachable!("token for type: {:?}", kind),
        };
        Ok(ty)
    }
}