use itertools::Itertools;

use crate::{error::{ErrorKind, Result, ThrowablePosition}, lexer::{Literal, Span, Token}, parser::{Expression, ExpressionKind, Parser}};

use super::BlockDefinition;

#[derive(Default)]
pub struct Range;

impl BlockDefinition for Range {
    fn id(&self) -> &str {
        "range"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Range expects exactly 2 operands".to_string()));
        }

        let start = operands.remove(0);
        let end = operands.remove(0);
        let ExpressionKind::Literal(Literal::Int(start)) = start.kind else {
            return Err(span.error(ErrorKind::Syntax, "Parameter of range should be integer".to_string()));
        };        
        let ExpressionKind::Literal(Literal::Int(end)) = end.kind else {
            return Err(span.error(ErrorKind::Syntax, "Parameter of range should be integer".to_string()));
        };
        Ok(ExpressionKind::Range(start, end))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}