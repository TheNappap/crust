
use itertools::Itertools;

use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        ExpressionKind, Parser, Signature, Type, Expression
    }, utils::{Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Call;

impl Call {
    pub fn parse_call(&self, span: &Span, path: Expression, params: Vec<Expression>, input: Option<Expression>) -> Result<ExpressionKind> {
        let path = match path.kind {
            ExpressionKind::Path(path) => path,
            ExpressionKind::Symbol(symbol) => symbol.name.into(),
            _ => return span.syntax("Expected path expression".into()),
        };

        let ty = match path.element_count() {
            1 => None,
            2 => {
                let ty = if path.is_inferred() {
                    Type::Inferred
                } else {
                    Type::Named(path.elements().last().unwrap().name().unwrap().to_owned())
                };
                Some(ty)
            }
            _ => return span.syntax("Expected proper path".into()),
        };

        let mut params: Vec<_> = params;
        if let Some(input) = input {
            params.insert(0, input);
        }
        Ok(ExpressionKind::Call(Signature::new(ty, path, vec![], Type::Inferred), params))
    }
}

impl BlockDefinition for Call {
    fn id(&self) -> &str {
        "call"
    }

    fn parse(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        use Delimeter::*;
        assert!(body.is_empty());
        assert!(matches!(header.last(), Some(Token { kind: TokenKind::Group(Parens, _), .. })));
        assert!(header.len() > 1);

        let Some(Token { kind: TokenKind::Group(Parens, params), .. }) = header.pop() else {
            return span.syntax("Badly formed call expression".into())
        };
        let params = parser.iter_expression(params).try_collect()?;

        let mut exprs: Vec<_> = parser.iter_expression(header).try_collect()?;
        match exprs.len() {
            1 => self.parse_call(span, exprs.remove(0), params, None),
            2 => {
                self.parse_call(span, exprs.remove(0), params,Some(exprs.remove(0)))
            },
            _ => return span.syntax("Badly formed call expression".into()),
        }
    }
    
    fn parse_chained(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        use Delimeter::*;
        assert!(body.is_empty());        
        assert!(header.len() > 2);
        assert!(matches!(header.last(), Some(Token { kind: TokenKind::Group(Parens, _), .. })));

        let Some(Token { kind: TokenKind::Group(Parens, params), .. }) = header.pop() else {
            return span.syntax("Badly formed call expression".into())
        };
        let params = parser.iter_expression(params).try_collect()?;

        let mut exprs: Vec<_> = parser.iter_expression(header).try_collect()?;
        assert!(exprs.len() == 1);
        
        self.parse_call(span, exprs.remove(0), params, Some(input))
    }
}
