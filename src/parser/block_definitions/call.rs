
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

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        use TokenKind::*;
        use Delimeter::*;
        let (ty, name, tokens) = match header.as_slice() {
            [Token{kind: Ident(name), ..}, Token{kind: Group(Parens, tokens), ..}] => (None, name.to_owned(), tokens),
            [Token{kind: Ident(type_name), ..}, Token{kind: ColonColon, ..}, Token{kind: Ident(name), ..}, Token{kind: Group(Parens, tokens), ..}] => 
                if type_name == "_" {
                    (Some(Type::Inferred), name.to_owned(), tokens)
                } else {
                    (Some(Type::Named(type_name.to_owned())), type_name.to_owned() + "::" + name, tokens)
                }
            _ => return Err(span.error(ErrorKind::Syntax, "Badly formed call expression".to_string())),
        };

        let exprs = parser.iter_expression(tokens.clone()).try_collect()?;
        Ok(ExpressionKind::Call(Signature::new(ty, &name, vec![], Type::Inferred), exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
