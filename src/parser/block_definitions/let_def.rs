use crate::{error::{Error, Result}, lexer::{Block, Token, Value}, parser::{
        syntax_tree::{Expression},
        Parser, Literal, Type,
    }};

use super::BlockDefinition;

pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Symbol('=')) => match tokens.next() {
                    Some(Token::Value(Value::Int(i))) => Ok( Expression::Let(id, Box::new(Expression::Literal(Literal::Int(i))) )),
                    Some(Token::Value(Value::Float(f))) => Ok( Expression::Let(id, Box::new(Expression::Literal(Literal::Float(f))) )),
                    Some(Token::Value(Value::String(s))) => Ok( Expression::Let(id, Box::new(Expression::Literal(Literal::String(s))) )),
                    Some(Token::Ident(name)) => Ok( Expression::Let(id, Box::new(Expression::Symbol(name, Type::Inferred)) )),
                    _ => Err(Error::syntax("Expected a value in assignment".to_string(), 0).into()),
                }
                _ => Err(Error::syntax("Expected '='".to_string(), 0).into()),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as variable name".to_string(), 0).into())
            }
        }
    }
}
