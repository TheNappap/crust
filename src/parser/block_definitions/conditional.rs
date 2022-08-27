use crate::{lexer::{Block, Literal}, parser::{Parser, Expression, Type}, error::{Result, Error}};

use super::BlockDefinition;

pub struct If;

impl BlockDefinition for If {
    fn id(&self) -> &str {
        "if"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let condition = parser.parse_expression(block.header)?;
        let body = block.body
            .into_iter()
            .map(|b| parser.parse_block_expression(b))
            .collect::<Result<_>>()?;
        Ok(Expression::If(Box::new(condition), body))
    }
}