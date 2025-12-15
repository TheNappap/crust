use std::vec;

use crate::{utils::{ErrorKind, Result, ThrowablePosition}, lexer::{Span, Token, TokenKind}, parser::{Expression, ExpressionKind, Parser, syntax_tree::fn_expr}};

use super::BlockDefinition;

#[derive(Default)]
pub struct Trait;

impl BlockDefinition for Trait {
    fn id(&self) -> &str {
        "trait"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return Err(span.error(ErrorKind::Syntax, "Expected symbol as type name".to_string()));
        };

        let mut sigs = vec![];
        let mut fns = vec![];
        for expr in parser.iter_statement(body) {
            match expr?.kind {
                ExpressionKind::Fn(fun) => fns.push(fun),
                ExpressionKind::Signature(sig) => sigs.push(sig),
                _ => return Err(span.error(ErrorKind::Syntax, "Unexpected block in trait".to_string())),
            }
        }

        Ok(ExpressionKind::Trait(fn_expr::Trait::new(name.clone(), sigs, fns)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}