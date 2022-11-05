use itertools::Itertools;

use crate::{error::{Error, Result}, lexer::{Token, Delimeter}, parser::{
        syntax_tree::{Expression},
        Parser, Type,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Array;

impl BlockDefinition for Array {
    fn id(&self) -> &str {
        "array"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        if let Some(Token::Group(Delimeter::Brackets, tokens)) = header.first() {
            let list = parser.parse_list(tokens.clone())
                .into_iter()
                .map(|tokens|parser.parse_expression(tokens))
                .try_collect()?;
            Ok(Expression::Array(list))
        } else {
            Err(Error::syntax("Expected array in brackets".to_string(), 0))
        }
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Index;

impl BlockDefinition for Index {
    fn id(&self) -> &str {
        "index"
    }

    fn parse(&self, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(body.is_empty());
        if let Some(Token::Group(Delimeter::Brackets, tokens)) = header.pop() {
            let index: Vec<_> = parser.parse_list(tokens)
                .into_iter()
                .map(|tokens| parser.parse_expression(tokens))
                .try_collect()?;
            if index.len() != 1 {
                return Err(Error::syntax("Expected exactly one index".to_string(), 0));
            }
            let index = index[0].clone();

            let collection = parser.parse_expression(header)?;

            Ok(Expression::Index(Box::new(collection), Box::new(index), Type::Inferred, 0))
        } else {
            return Err(Error::syntax("Expected brackets at the end of index block".to_string(), 0));
        }
    }

    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
