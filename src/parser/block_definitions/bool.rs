use crate::{lexer::{Block, Literal}, parser::{Parser, Expression, Type}, error::{Result, Error}};

use super::BlockDefinition;

pub struct True;

impl BlockDefinition for True {
    fn id(&self) -> &str {
        "true"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        Ok(Expression::Literal(Literal::Bool(1)))
    }
}

pub struct False;

impl BlockDefinition for False {
    fn id(&self) -> &str {
        "false"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        Ok(Expression::Literal(Literal::Bool(0)))
    }
}