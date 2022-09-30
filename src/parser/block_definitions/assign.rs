use itertools::Itertools;

use crate::{error::{Error, Result}, lexer::{Block, Token, Operator}, parser::{
        syntax_tree::{Expression},
        Parser, Type,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let mut tokens = header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Operator(Operator::Eq)) => {
                    let expression = parser.parse_expression(tokens.collect_vec())?;
                    Ok( Expression::Let(id, Box::new(expression), Type::Inferred) )
                }
                _ => Err(Error::syntax("Expected '='".to_string(), 0)),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as variable name".to_string(), 0))
            }
        }
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Mut;

impl BlockDefinition for Mut {
    fn id(&self) -> &str {
        "mut"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let mut tokens = header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Operator(Operator::Eq)) => {
                    let expression = parser.parse_expression(tokens.collect_vec())?;
                    Ok( Expression::Mut(id, Box::new(expression)) )
                }
                _ => Err(Error::syntax("Expected '='".to_string(), 0)),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as variable name".to_string(), 0))
            }
        }
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
