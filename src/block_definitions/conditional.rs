


use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{Expression, ExpressionKind, Parser, Type, BlockTag}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;

#[derive(Default)]
pub struct If;

impl BlockDefinition for If {
    fn id(&self) -> BlockTag {
        BlockTag::from("if")
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let condition = parser.parse_expression(header)?;
        let body = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::If(Box::new(condition), body, None, Type::Inferred))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}

#[derive(Default)]
pub struct Else;

impl BlockDefinition for Else {
    fn id(&self) -> BlockTag {
        BlockTag::from("else")
    }

    fn parse(&self, span: &Span, _header: Vec<Token>, _body: Vec<Token>, _parser: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpectedly no input, block needs input".into())
    }

    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        let else_body = parser.iter_statement(body).try_collect()?;

        let if_else_expr = match input.kind {
            ExpressionKind::If(condition, if_body, None, ty) => ExpressionKind::If(condition.clone(), if_body.clone(), Some(else_body), ty),
            _ => return span.syntax("Expected if expression before else".into())
        };
        Ok(if_else_expr)
    }
}