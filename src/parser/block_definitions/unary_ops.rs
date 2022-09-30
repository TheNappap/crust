use crate::{lexer::{Block, Token}, parser::{Parser, Expression, Type, UnOpKind}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Negate;

impl BlockDefinition for Negate {
    fn id(&self) -> &str {
        "neg"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let operand = parser.parse_expression(header);
        Ok(Expression::UnOp(UnOpKind::Neg, Box::new(operand?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}