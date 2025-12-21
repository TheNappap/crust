use std::vec;

use crate::{lexer::{Span, Token, TokenKind}, parser::{BlockTag, Expression, ExpressionKind, Parser, Trait}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;

#[derive(Default)]
pub struct TraitBlock;

impl BlockDefinition for TraitBlock {
    fn id(&self) -> BlockTag {
        BlockTag::from("trait")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return span.syntax("Expected symbol as type name".into());
        };

        let mut sigs = vec![];
        let mut fns = vec![];
        for expr in parser.iter_statement(body) {
            match expr?.kind {
                ExpressionKind::Fn(fun) => fns.push(fun),
                ExpressionKind::Signature(sig) => sigs.push(sig),
                _ => return span.syntax("Unexpected block in trait".into()),
            }
        }

        Ok(ExpressionKind::Trait(Trait::new(name.clone(), sigs, fns)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}