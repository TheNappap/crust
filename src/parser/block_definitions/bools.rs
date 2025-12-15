use crate::{lexer::{Literal, Token, Span}, parser::{Parser, Expression, ExpressionKind}, utils::{Result, ThrowablePosition, ErrorKind}};

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
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
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
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}