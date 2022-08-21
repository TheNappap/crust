use itertools::Itertools;

use crate::{error::{Error, Result}, lexer::{Block, Token}, parser::{
        syntax_tree::{Expression},
        Parser, Type,
    }};

use super::BlockDefinition;

pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Symbol('=')) => {
                    let expression = parser.parse_expression(tokens.collect_vec())?;
                    Ok( Expression::Let(id, Box::new(expression), Type::Inferred) )
                }
                _ => Err(Error::syntax("Expected '='".to_string(), 0).into()),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as variable name".to_string(), 0).into())
            }
        }
    }
}
