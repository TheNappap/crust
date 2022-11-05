use crate::{lexer::{Token}, parser::{Parser, Expression, Type, UnOpKind}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Negate;

impl BlockDefinition for Negate {
    fn id(&self) -> &str {
        "neg"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let operand = parser.parse_expression(header);
        Ok(Expression::UnOp(UnOpKind::Neg, Box::new(operand?), Type::Inferred))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}