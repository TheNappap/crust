use crate::{lexer::{Token, Span}, parser::{Parser, Expression, Type, UnOpKind, ExpressionKind}, error::{Result, ErrorKind, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Negate;

impl BlockDefinition for Negate {
    fn id(&self) -> &str {
        "neg"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let operand = parser.parse_expression(header);
        Ok(ExpressionKind::UnOp(UnOpKind::Neg, Box::new(operand?), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}