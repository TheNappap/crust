use itertools::Itertools;

use crate::{error::{Error, Result}, lexer::{Block, Token, Operator, Delimeter}, parser::{
        syntax_tree::{Expression},
        Parser,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Array;

impl BlockDefinition for Array {
    fn id(&self) -> &str {
        "array"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        if let Some(Token::Group(Delimeter::Brackets, tokens)) = header.first() {
            let list = parser.parse_list(tokens.clone())
                .contents.into_iter()
                .map(|tokens|parser.parse_expression(tokens))
                .try_collect()?;
            Ok(Expression::Array(list))
        } else {
            Err(Error::syntax("Expected array in brackets".to_string(), 0))
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
