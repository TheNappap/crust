
use itertools::Itertools;

use crate::{lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        BlockTag, Expression, ExpressionKind, Parser, Type
    }, utils::{ErrorKind, Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Array;

impl BlockDefinition for Array {
    fn id(&self) -> BlockTag {
        BlockTag::from("array")
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let list = parser.iter_expression(header).try_collect()?;
        Ok(ExpressionKind::Array(list))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Index;

impl BlockDefinition for Index {
    fn id(&self) -> BlockTag {
        BlockTag::from("index")
    }

    fn parse(&self, _: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        assert!(!header.is_empty());

        let first_token = header.pop().unwrap();
        if let TokenKind::Group(Delimeter::Brackets, tokens) = first_token.kind {
            let index: Vec<_> = parser.iter_expression(tokens).try_collect()?;
            if index.len() != 1 {
                return Err(first_token.span.error(ErrorKind::Syntax, "Expected exactly one index".to_string()));
            }
            let index = index[0].clone();

            let collection = parser.parse_expression(header)?;

            Ok(ExpressionKind::Index(Box::new(collection), Box::new(index), Type::Inferred, 0))
        } else {
            return Err(first_token.span.error(ErrorKind::Syntax, "Expected brackets at the end of index block".to_string()));
        }
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
