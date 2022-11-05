

use crate::{error::{Error, Result}, lexer::{Token}, parser::{
        syntax_tree::{Expression},
        Parser,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let (id, ty) = match parser.parse_expression(header)? {
            Expression::Symbol(id, ty) => (id, ty),
            _ => {
                return Err(Error::syntax("Expected an identifier as variable name".to_string(), 0));
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(Expression::Let(id, Box::new(value), ty))
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Mut;

impl BlockDefinition for Mut {
    fn id(&self) -> &str {
        "mut"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let id = match parser.parse_expression(header)? {
            Expression::Symbol(id, _) => id,
            _ => {
                return Err(Error::syntax("Expected an identifier as variable name".to_string(), 0));
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(Expression::Mut(id, Box::new(value)))
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
