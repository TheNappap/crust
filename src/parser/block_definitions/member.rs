use itertools::Itertools;

use crate::{
    error::{Result, Error},
    lexer::{Token},
    parser::{
        syntax_tree::{Expression},
        Parser, Type
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Field;

impl BlockDefinition for Field {
    fn id(&self) -> &str {
        "field"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(body.is_empty());
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Field expression needs exactly 2 operands".to_string(), 0));
        }

        let operands: Vec<_> = token_list.into_iter()
            .map(|tokens| {
                if let Expression::Symbol(name, _) = parser.parse_expression(tokens)? {
                    return Ok(name);
                }
                Err(Error::syntax("Field expression expected symbol expressions".to_string(), 0))
            })
            .try_collect()?;

        Ok(Expression::Field(operands[0].clone(), operands[1].clone(), Type::Inferred, -1))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}