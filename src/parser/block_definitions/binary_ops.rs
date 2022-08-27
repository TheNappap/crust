use crate::{lexer::{Block}, parser::{Parser, Expression, Type}, error::{Result, Error}};

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
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Add(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
}

pub struct Subtract;

impl BlockDefinition for Subtract {
    fn id(&self) -> &str {
        "sub"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let token_list = parser.parse_list(block.header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Sub(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
}

pub struct Multiply;

impl BlockDefinition for Multiply {
    fn id(&self) -> &str {
        "mul"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let token_list = parser.parse_list(block.header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Mul(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
}
pub struct Divide;

impl BlockDefinition for Divide {
    fn id(&self) -> &str {
        "div"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let token_list = parser.parse_list(block.header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Div(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
}