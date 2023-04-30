


use crate::{lexer::{Token, Span}, parser::{Parser, Expression, ExpressionKind}, error::{Result, ErrorKind, ThrowablePosition}};

use super::BlockDefinition;

#[derive(Default)]
pub struct If;

impl BlockDefinition for If {
    fn id(&self) -> &str {
        "if"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let condition = parser.parse_expression(header)?;
        let body = parser.parse_group(body)?;
        Ok(ExpressionKind::If(Box::new(condition), body, None))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Else;

impl BlockDefinition for Else {
    fn id(&self) -> &str {
        "else"
    }

    fn parse(&self, span: &Span, _header: Vec<Token>, _body: Vec<Token>, _parser: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpectedly no input, block needs input".to_string()))
    }

    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        let else_body = parser.parse_group(body)?;

        let if_else_expr = match input.kind {
            ExpressionKind::If(condition, if_body, None) => ExpressionKind::If(condition.clone(), if_body.clone(), Some(else_body)),
            _ => return Err(span.error(ErrorKind::Syntax, "Expected if expression before else".to_string()))
        };
        Ok(if_else_expr)
    }
}