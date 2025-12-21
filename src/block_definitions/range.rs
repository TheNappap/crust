
use crate::{block_definitions::OperatorBlockDefintion, lexer::{Literal, Span}, parser::{BlockTag, Expression, ExpressionKind, OperatorKind}, utils::{Result, ThrowablePosition}};


#[derive(Default)]
pub struct Range;

impl OperatorBlockDefintion for Range {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Range)
    }

    fn parse_binary_operator(&self, span: &Span, start: Expression, end: Expression) -> Result<ExpressionKind> {
        let ExpressionKind::Literal(Literal::Int(start)) = start.kind else {
            return span.syntax("Parameter of range should be integer".into());
        };        
        let ExpressionKind::Literal(Literal::Int(end)) = end.kind else {
            return span.syntax("Parameter of range should be integer".into());
        };
        Ok(ExpressionKind::Range(start, end))
    }
}