use crate::{lexer::{Token, Span}, parser::{Parser, Expression, ExpressionKind}, utils::{Result, ThrowablePosition, ErrorKind}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Iter;

impl BlockDefinition for Iter {
    fn id(&self) -> &str {
        "iter"
    }

    fn parse(&self, _: &Span, header: Vec<Token>, _: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let iter = parser.parse_expression(header)?;
        Ok(ExpressionKind::Iter(Box::new(iter), vec![], 0))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}