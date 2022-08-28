use crate::{lexer::{Block, Literal, Token}, parser::{Parser, Expression}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct True;

impl BlockDefinition for True {
    fn id(&self) -> &str {
        "true"
    }

    fn parse(&self, _header: Vec<Token>, _body: Vec<Block>, _parser: &Parser) -> Result<Expression> {
        Ok(Expression::Literal(Literal::Bool(1)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct False;

impl BlockDefinition for False {
    fn id(&self) -> &str {
        "false"
    }

    fn parse(&self, _header: Vec<Token>, _body: Vec<Block>, _parser: &Parser) -> Result<Expression> {
        Ok(Expression::Literal(Literal::Bool(0)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}