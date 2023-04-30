use itertools::Itertools;

use crate::{error::{Result, ErrorKind, ThrowablePosition}, lexer::{Token, Delimeter, TokenKind, Span}, parser::{
        syntax_tree::{Expression},
        Parser, Type, ExpressionKind,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Array;

impl BlockDefinition for Array {
    fn id(&self) -> &str {
        "array"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        if let Some(TokenKind::Group(Delimeter::Brackets, tokens)) = header.first().map(|t|&t.kind) {
            let list = parser.parse_list(tokens.clone())
                .into_iter()
                .map(|tokens|parser.parse_expression(tokens))
                .try_collect()?;
            Ok(ExpressionKind::Array(list))
        } else {
            Err(span.error(ErrorKind::Syntax, "Expected array in brackets".to_string()))
        }
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Index;

impl BlockDefinition for Index {
    fn id(&self) -> &str {
        "index"
    }

    fn parse(&self, _: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        assert!(!header.is_empty());

        let first_token = header.pop().unwrap();
        if let TokenKind::Group(Delimeter::Brackets, tokens) = first_token.kind {
            let index: Vec<_> = parser.parse_list(tokens.clone())
                .into_iter()
                .map(|tokens| parser.parse_expression(tokens))
                .try_collect()?;
            if index.len() != 1 {
                return Err(first_token.span.error(ErrorKind::Syntax, "Expected exactly one index".to_string()));
            }
            let index = index[0].clone();

            let collection = parser.parse_expression(header)?;

            Ok(ExpressionKind::Index(Box::new(collection), Box::new(index), Type::Inferred, 0))
        } else {
            return Err(first_token.span.error(ErrorKind::Syntax, "Expected brackets at the end of index block".to_string()));
        }
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
