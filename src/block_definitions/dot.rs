
use itertools::Itertools;

use crate::{block_definitions::{BlockDefinition, OperatorBlockDefintion, call::Call, data::Field}, lexer::{Delimeter, Span, Token, TokenKind}, parser::{BlockTag, Expression, ExpressionKind, OperatorKind, Parser}, utils::{ErrorKind, Result, ThrowablePosition}};

#[derive(Default)]
pub struct Dot;

impl OperatorBlockDefintion for Dot {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Dot)
    }

    fn s_override_parsing(&self) -> bool {
        true
    }

    fn s_parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let token_list = parser.split_list(header.clone()).collect_vec();
        match token_list.len() {
            1 => Call.parse(span, header, body, parser),
            2 => {
                match token_list[1][..].iter().map(|t|&t.kind).collect_vec().as_slice() {
                    [TokenKind::Ident(_)] => Field.parse(span, header, vec![], parser),
                    [TokenKind::Ident(name), TokenKind::Group(Delimeter::Parens, tokens)] => {
                        let mut params = token_list[0].clone();
                        params.extend(tokens.clone());
                        let header = vec![TokenKind::Underscore, TokenKind::ColonColon, TokenKind::Ident(name.to_owned()), TokenKind::Group(Delimeter::Parens, params)];
                        Call.parse(span, header.into_iter().map(|t|Token::new(t, span.clone())).collect(), vec![], parser)
                    }
                    _ => Err(span.error(ErrorKind::Syntax, "Badly formed . expression".to_string())),
                }
            }
            _ => return Err(span.error(ErrorKind::Syntax, "Dot operator expects 1 or 2 operands".to_string()))
        }
    }
    
    fn s_parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let token_list = parser.split_list(header.clone()).collect_vec();
        if token_list.len() != 1 {
            return Err(span.error(ErrorKind::Syntax, "Dot operator expects 1 or 2 operands".to_string()))
        }

        match token_list[0][..].iter().map(|t|&t.kind).collect_vec().as_slice() {
            [TokenKind::Ident(_)] => Field.parse_chained(span, header, vec![], input, parser),
            [TokenKind::Ident(_), TokenKind::Group(Delimeter::Parens, _)] => {
                Call.parse_chained(span, header, body, input, parser)
            }
            _ => Err(span.error(ErrorKind::Syntax, "Badly formed . expression".to_string())),
        }
    }
}