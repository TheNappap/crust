use crate::{lexer::{Token}, parser::{Parser, Expression, Type, BinOpKind}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> &str {
        "add"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Add, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Subtract;

impl BlockDefinition for Subtract {
    fn id(&self) -> &str {
        "sub"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Sub, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Multiply;

impl BlockDefinition for Multiply {
    fn id(&self) -> &str {
        "mul"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Mul, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Divide;

impl BlockDefinition for Divide {
    fn id(&self) -> &str {
        "div"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Div, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Equals;

impl BlockDefinition for Equals {
    fn id(&self) -> &str {
        "eq"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Eq, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct NotEquals;

impl BlockDefinition for NotEquals {
    fn id(&self) -> &str {
        "neq"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        let mut operands = token_list.into_iter().map(|tokens| parser.parse_expression(tokens));
        Ok(Expression::BinOp(BinOpKind::Neq, Box::new(operands.next().unwrap()?), Box::new(operands.next().unwrap()?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}