
use std::collections::HashMap;

use crate::{lexer::{Token, Delimeter}, error::{Error, Result}};

use super::field_map::FieldMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Inferred,
    Named(String),
    Array(Box<Type>, usize),
    Struct(String, FieldMap<String, Type>),
    Enum(String, HashMap<String, usize>),
    Iter(Box<Type>),
}

impl Type {
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            Int | Float | Bool | String | Iter(_) => 8,
            Type::Array(ty, len) => ty.size()*(*len as u32),
            Type::Struct(_, types) => types.values().map(Type::size).sum(),
            Type::Enum(_, _) => Int.size(),
            Void => 0,
            Inferred | Named(_) => unreachable!(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Bool => "Bool",
            Type::String => "String",
            Type::Void => "Void",
            Type::Inferred => "Inferred",
            Type::Named(name) => name,
            Type::Array(_, _) => todo!(),
            Type::Struct(name, _) => name,
            Type::Enum(name, _) => name,
            Type::Iter(_) => todo!(),
        }
    }

    pub fn from(token: Token) -> Result<Self> {
        use crate::lexer::Operator::*;
        use crate::lexer::Literal::*;
        use Token::*;
        let ty = match token {
            Ident(ty) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                name => Type::Named(name.to_owned())
            }
            Group(Delimeter::Brackets, tokens) => {
                match tokens.as_slice() {
                    [Ident(name), Operator(Semicolon), Literal(Int(n))] =>
                        Type::Array(Box::new(Type::from(Ident(name.clone()))?), *n as usize),
                    _ => return Err(Error::syntax("Unknown type".into(), 0))
                }
            }
            t => unreachable!("token for type: {:?}", t),
        };
        Ok(ty)
    }
}