use crate::{lexer::{Literal, Span, Token}, parser::{Expression, ExpressionKind, Parser}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct True;

impl BlockDefinition for True {
    fn id(&self) -> &str {
        "true"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, _parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        assert!(body.is_empty());
        Ok(ExpressionKind::Literal(Literal::Bool(true)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}

#[derive(Default)]
pub struct False;

impl BlockDefinition for False {
    fn id(&self) -> &str {
        "false"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, _parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        assert!(body.is_empty());
        Ok(ExpressionKind::Literal(Literal::Bool(false)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}