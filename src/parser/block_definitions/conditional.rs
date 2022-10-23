use itertools::Itertools;

use crate::{lexer::{Block, Token}, parser::{Parser, Expression}, error::{Result, Error}};

use super::BlockDefinition;

#[derive(Default)]
pub struct If;

impl BlockDefinition for If {
    fn id(&self) -> &str {
        "if"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let condition = parser.parse_expression(header)?;
        let body = body
            .into_iter()
            .map(|b| parser.parse_block_expression(b))
            .try_collect()?;
        Ok(Expression::If(Box::new(condition), body, None))
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Else;

impl BlockDefinition for Else {
    fn id(&self) -> &str {
        "else"
    }

    fn parse(&self, _header: Vec<Token>, _body: Vec<Block>, _parser: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpectedly no input, block needs input".to_string(), 0))
    }

    fn parse_chained(&self, header: Vec<Token>, body: Vec<Block>, input: Expression, parser: &Parser) -> Result<Expression> {
        assert!(header.is_empty());
        let else_body = body
            .into_iter()
            .map(|b| parser.parse_block_expression(b))
            .try_collect()?;

        let if_else_expr = match input {
            Expression::If(condition, if_body, None) => Expression::If(condition, if_body, Some(else_body)),
            _ => return Err(Error::syntax("Expected if expression before else".to_string(), 0))
        };
        Ok(if_else_expr)
    }
}