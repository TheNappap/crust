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
pub struct Struct;

impl BlockDefinition for Struct {
    fn id(&self) -> &str {
        "struct"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(header.len() == 1);
        let Some(Token::Ident(name)) = header.first() else {
            return Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0));
        };

        let types = parser.parse_list(body).into_iter()
            .map(|tokens| parser.parse_parameter(tokens).map(|(_, t)|t))
            .try_collect()?;

        Ok(Expression::Struct(name.to_string(), Type::Struct(types)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}