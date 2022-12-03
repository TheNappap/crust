use std::collections::{BTreeMap};

use crate::lexer::Token;


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
    Struct(BTreeMap<String, Type>),
    Iter(Box<Type>),
}

impl Type {
    pub fn size(&self) -> u32 {
        use Type::*;
        match self {
            Int | Float | Bool | String | Iter(_) => 8,
            Type::Array(ty, len) => ty.size()*(*len as u32),
            Type::Struct(types) => types.values().map(Type::size).sum(),
            Void => 0,
            Inferred | Named(_) => unreachable!(),
        }
    }
}

impl From<Token> for Type {
    fn from(token: Token) -> Self {
        match token {
            Token::Ident(ty) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                name => Type::Named(name.to_owned())
            }
            t => unreachable!("token for type: {:?}", t),
        }
    }
}