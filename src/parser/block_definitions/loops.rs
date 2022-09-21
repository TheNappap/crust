use crate::{lexer::{Block, Token}, parser::{Parser, Expression}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct While;

impl BlockDefinition for While {
    fn id(&self) -> &str {
        "while"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let condition = parser.parse_expression(header)?;
        let body = body
            .into_iter()
            .map(|b| parser.parse_block_expression(b))
            .collect::<Result<_>>()?;
        Ok(Expression::While(Box::new(condition), body))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}