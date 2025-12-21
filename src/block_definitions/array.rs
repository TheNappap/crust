
use itertools::Itertools;

use crate::{lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        BlockTag, Expression, ExpressionKind, Parser, Type
    }, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Array;

impl BlockDefinition for Array {
    fn id(&self) -> BlockTag {
        BlockTag::from("array")
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        // TODO only allow brackets for params
        let list = parser.iter_expression(header).try_collect()?;
        Ok(ExpressionKind::Array(list))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}


#[derive(Default)]
pub struct Index;

impl Index {
    pub fn parse_index(&self, span: &Span, collection: Expression, mut params: Vec<Expression>) -> Result<ExpressionKind> {
        if params.len() != 1 {
            return span.syntax("Expected exactly one index".into());
        }
        let index = params.remove(0);
        Ok(ExpressionKind::Index(Box::new(collection), Box::new(index), Type::Inferred, 0))
    }
}

impl BlockDefinition for Index {
    fn id(&self) -> BlockTag {
        BlockTag::from("index")
    }

    fn parse(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        assert!(!header.is_empty());

        let bracket_token = header.pop().unwrap();
        if let TokenKind::Group(Delimeter::Brackets, params) = bracket_token.kind {
            let collection = parser.parse_expression(header)?;
            let params = parser.iter_expression(params).try_collect()?;
            self.parse_index(span, collection, params)
        } else {
            return span.syntax("Expected brackets at the end of index block".into());
        }
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}
