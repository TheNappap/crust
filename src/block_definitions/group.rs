

use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{Expression, ExpressionKind, Parser, Type, BlockTag}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Group;

impl BlockDefinition for Group {
    fn id(&self) -> BlockTag {
        BlockTag::from("group")
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        let exprs = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::Group(exprs, Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}
