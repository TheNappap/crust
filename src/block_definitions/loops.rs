


use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{Expression, ExpressionKind, Parser}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct While;

impl BlockDefinition for While {
    fn id(&self) -> &str {
        "while"
    }

    fn parse(&self, _: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let condition = parser.parse_expression(header)?;
        let body = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::While(Box::new(condition), body))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}

#[derive(Default)]
pub struct For;

impl BlockDefinition for For {
    fn id(&self) -> &str {
        "for"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpectedly no input, block needs input".into())
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(s) => s,
            _ => return span.syntax("Expected symbol for loop variable".into())
        };
        let body = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::Fold(Box::new(input), symbol, None, body))
    }
}

#[derive(Default)]
pub struct Fold;

impl BlockDefinition for Fold {
    fn id(&self) -> &str {
        "fold"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpectedly no input, block needs input".into())
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let mut header: Vec<_> = parser.iter_expression(header).try_collect()?;
        let init_expression = header.remove(0);
        let acc_symbol = match header.remove(0).kind {
            ExpressionKind::Symbol(s) => s.clone(),
            _ => return span.syntax("Expected symbol for accumulator variable".into())
        };
        let symbol = match header.remove(0).kind {
            ExpressionKind::Symbol(s) => s.clone(),
            _ => return span.syntax("Expected symbol for loop variable".into())
        };
        let body = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::Fold(Box::new(input), symbol, Some((Box::new(init_expression), acc_symbol)), body))
    }
}