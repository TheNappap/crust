

use itertools::Itertools;

use crate::{utils::{ErrorKind, Result, ThrowablePosition}, lexer::{Span, Token}, parser::{Expression, ExpressionKind, Parser}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Group;

impl BlockDefinition for Group {
    fn id(&self) -> &str {
        "group"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.is_empty());
        let exprs = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::Group(exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
