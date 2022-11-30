use std::collections::HashMap;

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
    Struct(HashMap<String, Type>),
    Iter(Box<Type>),
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