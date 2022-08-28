use crate::{lexer::{Block, Token}, parser::{Parser, Expression, Type}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> &str {
        "add"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Add(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Subtract;

impl BlockDefinition for Subtract {
    fn id(&self) -> &str {
        "sub"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Sub(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Multiply;

impl BlockDefinition for Multiply {
    fn id(&self) -> &str {
        "mul"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Mul(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Divide;

impl BlockDefinition for Divide {
    fn id(&self) -> &str {
        "div"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.contents.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.contents.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::Div(Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}