use crate::{lexer::{Block, Token, Literal}, parser::{Parser, Expression, Type}, error::{Result, Error}};

use super::BlockDefinition;

pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> &str {
        "add"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let token_list = parser.parse_list(block.header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0).into());
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Add(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
}