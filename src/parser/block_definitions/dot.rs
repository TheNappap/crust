
use itertools::Itertools;

use crate::{utils::{ErrorKind, Result, ThrowablePosition}, lexer::{Delimeter, Operator, Span, Token, TokenKind}, parser::{Expression, ExpressionKind, Parser, block_definitions::call::Call}};

use super::{BlockDefinition, data::Field};

#[derive(Default)]
pub struct Dot;

impl BlockDefinition for Dot {
    fn id(&self) -> &str {
        "dot"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.len() == 0);
        let token_list = parser.split_list(header.clone()).collect_vec();
        match token_list.len() {
            1 => Call.parse(span, header, body, parser),
            2 => {
                match token_list[1][..].iter().map(|t|&t.kind).collect_vec().as_slice() {
                    [TokenKind::Ident(_)] => Field.parse(span, header, vec![], parser),
                    [TokenKind::Ident(name), TokenKind::Group(Delimeter::Parens, tokens)] => {
                        let mut params = token_list[0].clone();
                        params.extend(tokens.clone());
                        let header = vec![TokenKind::Ident("_".to_owned()), TokenKind::Operator(Operator::ColonColon), TokenKind::Ident(name.to_owned()), TokenKind::Group(Delimeter::Parens, params)];
                        Call.parse(span, header.into_iter().map(|t|Token::new(t, span.clone())).collect(), vec![], parser)
                    }
                    _ => Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string())),
                }
            }
            _ => return Err(span.error(ErrorKind::Syntax, "Dot operator expects 1 or 2 operands".to_string()))
        }
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}