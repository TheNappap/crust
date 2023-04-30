

use crate::{lexer::{Token, Span}, parser::{Parser, Expression, ExpressionKind}, error::{Result, ErrorKind, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Group;

impl BlockDefinition for Group {
    fn id(&self) -> &str {
        "group"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        let exprs = parser.parse_group(body)?;
        Ok(ExpressionKind::Group(exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
