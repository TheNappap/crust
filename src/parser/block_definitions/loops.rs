

use crate::{lexer::{Token}, parser::{Parser, Expression}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct While;

impl BlockDefinition for While {
    fn id(&self) -> &str {
        "while"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let condition = parser.parse_expression(header)?;
        let body = parser.parse_group(body)?;
        Ok(Expression::While(Box::new(condition), body))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct For;

impl BlockDefinition for For {
    fn id(&self) -> &str {
        "for"
    }

    fn parse(&self, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpectedly no input, block needs input".to_string(), 0))
    }
    
    fn parse_chained(&self, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<Expression> {
        let (var_name, var_type) = match parser.parse_expression(header)? {
            Expression::Symbol(s, t) => (s,t),
            _ => return Err(Error::syntax("Expected symbol for loop variable".to_string(), 0))
        };
        let body = parser.parse_group(body)?;
        Ok(Expression::For(Box::new(input), var_name, var_type, body))
    }
}