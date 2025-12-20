
use crate::{block_definitions::OperatorBlockDefintion, lexer::{Literal, Span}, parser::{BlockTag, Expression, ExpressionKind, OperatorKind}, utils::{ErrorKind, Result, ThrowablePosition}};


#[derive(Default)]
pub struct Range;

impl OperatorBlockDefintion for Range {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Range)
    }

    fn parse_binary_operator(&self, span: &Span, start: Expression, end: Expression) -> Result<ExpressionKind> {
        let ExpressionKind::Literal(Literal::Int(start)) = start.kind else {
            return Err(span.error(ErrorKind::Syntax, "Parameter of range should be integer".to_string()));
        };        
        let ExpressionKind::Literal(Literal::Int(end)) = end.kind else {
            return Err(span.error(ErrorKind::Syntax, "Parameter of range should be integer".to_string()));
        };
        Ok(ExpressionKind::Range(start, end))
    }
}