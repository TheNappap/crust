
use itertools::Itertools;

use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        ExpressionKind, Parser, Signature, Type, blocks::BlockTag, syntax_tree::Expression
    }, utils::{ErrorKind, Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Call;

impl BlockDefinition for Call {
    fn id(&self) -> BlockTag {
        BlockTag::from("call")
    }

    // TODO implement the ColonColon operator
    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        use TokenKind::*;
        use Delimeter::*;
        assert!(body.is_empty());
        let (ty, name, tokens) = match header.as_slice() {
            [Token{kind: Ident(name), ..}, Token{kind: Group(Parens, tokens), ..}] => (None, name.to_owned(), tokens),
            [Token{kind: Ident(type_name), ..}, Token{kind: ColonColon, ..}, Token{kind: Ident(name), ..}, Token{kind: Group(Parens, tokens), ..}] => {
                if type_name == "_" {
                    (Some(Type::Inferred), name.to_owned(), tokens)
                } else {
                    (Some(Type::Named(type_name.to_owned())), type_name.to_owned() + "::" + name, tokens)
                }
            }
            _ => return Err(span.error(ErrorKind::Syntax, "Badly formed call expression".to_string())),
        };

        let exprs = parser.iter_expression(tokens.clone()).try_collect()?;
        Ok(ExpressionKind::Call(Signature::new(ty, &name, vec![], Type::Inferred), exprs))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        use TokenKind::*;
        use Delimeter::*;
        assert!(body.is_empty());
        let (ty, name, tokens) = match header.as_slice() {
            [Token{kind: Ident(name), ..}, Token{kind: Group(Parens, tokens), ..}] => (Type::Inferred, name.to_owned(), tokens),
            _ => return Err(span.error(ErrorKind::Syntax, "Badly formed call expression".to_string())),
        };

        let mut exprs: Vec<_> = parser.iter_expression(tokens.clone()).try_collect()?;
        exprs.insert(0, input);
        Ok(ExpressionKind::Call(Signature::new(Some(ty), &name, vec![], Type::Inferred), exprs))
    }
}
