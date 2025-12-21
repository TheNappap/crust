use itertools::Itertools;

use crate::{lexer::{Span, Token, TokenKind}, parser::{Expression, ExpressionKind, Parser, Pattern, Type}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Match;

impl BlockDefinition for Match {
    fn id(&self) -> &str {
        "match"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let match_value = parser.parse_expression(header)?;
        let cases = parser.iter_block(body)
            .map_ok(|expr| match expr.kind {
                ExpressionKind::Case(pattern, exprs) => Ok((pattern.clone(), exprs.clone())),
                _ => span.syntax("Expected case expression in match expression".into())
            })
            .flatten()
            .try_collect()?;

        Ok(ExpressionKind::Match(Box::new(match_value), Type::Inferred, cases))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}

#[derive(Default)]
pub struct Case;

impl BlockDefinition for Case {
    fn id(&self) -> &str {
        "case"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let pattern = match header.iter().map(|t|&t.kind).collect_vec().as_slice() {
            [TokenKind::Underscore] => Pattern::Ident("_".to_owned()),
            [TokenKind::Ident(name)] => Pattern::Ident(name.to_owned()),
            [TokenKind::Ident(ty), TokenKind::ColonColon, TokenKind::Ident(name)] => Pattern::EnumVariant(ty.to_owned(), name.to_owned()),
            _ => return span.syntax("Failed to parse pattern in match expression".into()),
        };

        let exprs = parser.iter_statement(body).try_collect()?;
        Ok(ExpressionKind::Case(pattern, exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}
