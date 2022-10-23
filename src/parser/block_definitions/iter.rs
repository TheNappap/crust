use crate::{lexer::{Block, Token}, parser::{Parser, Expression}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Iter;

impl BlockDefinition for Iter {
    fn id(&self) -> &str {
        "iter"
    }

    fn parse(&self, header: Vec<Token>, _: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let iter = parser.parse_expression(header)?;
        Ok(Expression::Iter(Box::new(iter)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}