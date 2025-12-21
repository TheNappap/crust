use crate::{lexer::{Span, Token}, parser::{Expression, ExpressionKind, Parser, BlockTag}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Iter;

impl BlockDefinition for Iter {
    fn id(&self) -> BlockTag {
        BlockTag::from("iter")
    }

    fn parse(&self, _: &Span, header: Vec<Token>, _: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let iter = parser.parse_expression(header)?;
        Ok(ExpressionKind::Iter(Box::new(iter), vec![], 0))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}