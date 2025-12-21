
use itertools::Itertools;

use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        ExpressionKind, Parser, Signature, Type, BlockTag, Expression
    }, utils::{Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Call;

impl Call {
    fn parse_call(&self, span: &Span, path: ExpressionKind, params: Vec<Token>, input: Option<Expression>, parser: &Parser) -> Result<ExpressionKind> {
        let path = match path {
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

        let mut params: Vec<_> = parser.iter_expression(params).try_collect()?;
        if let Some(input) = input {
            params.insert(0, input);
        }
        Ok(ExpressionKind::Call(Signature::new(ty, path, vec![], Type::Inferred), params))
    }
}

impl BlockDefinition for Call {
    fn id(&self) -> BlockTag {
        BlockTag::from("call")
    }

    fn parse(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        use Delimeter::*;
        assert!(body.is_empty());
        assert!(matches!(header.last(), Some(Token { kind: TokenKind::Group(Parens, _), .. })));
        assert!(header.len() > 1);

        // TODO generalize parens and indexing as a postfix operator
        let Some(Token { kind: TokenKind::Group(Parens, params), .. }) = header.pop() else {
            return span.syntax("Badly formed call expression".into())
        };

        let exprs: Vec<_> = parser.iter_expression(header).try_collect()?;
        match exprs.len() {
            1 => self.parse_call(span, exprs[0].kind.clone(), params, None, parser),
            2 => {
                self.parse_call(span, exprs[1].kind.clone(), params,Some(exprs[0].clone()), parser)
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

        let exprs: Vec<_> = parser.iter_expression(header).try_collect()?;
        assert!(exprs.len() == 1);
        
        self.parse_call(span, exprs[0].kind.clone(), params, Some(input), parser)
    }
}
