use crate::{error::{Error, Result}, lexer::{Block, Token}, parser::{
        syntax_tree::{Expression},
        Parser,
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
                    Some(Token::Value(value)) => {
                        Ok(Expression::Let(id, value))
                    },
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
