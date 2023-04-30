
use itertools::Itertools;

use crate::{lexer::{Token, Delimeter, Operator, Span, TokenKind}, parser::{Parser, Expression, block_definitions::call::Call, ExpressionKind}, error::{Result, ErrorKind, ThrowablePosition}};

use super::{BlockDefinition, data::Field};

#[derive(Default)]
pub struct Dot;

impl BlockDefinition for Dot {
    fn id(&self) -> &str {
        "dot"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.len() == 0);
        let token_list = parser.parse_list(header.clone());
        if token_list.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Binary operator expects exactly 2 operands".to_string()));
        }

        match token_list[1][..].iter().map(|t|&t.kind).collect_vec().as_slice() {
            [TokenKind::Ident(_)] => Field.parse(span, header, vec![], parser),
            [TokenKind::Ident(name), TokenKind::Group(Delimeter::Parens, tokens)] => {
                let mut params = token_list[0].clone();
                params.extend(tokens.clone());
                let mut header = vec![TokenKind::Ident("_".to_owned()), TokenKind::Operator(Operator::ColonColon), TokenKind::Ident(name.to_owned())];
                header.push(TokenKind::Group(Delimeter::Parens, params));
                Call.parse(span, header.into_iter().map(|t|Token::new(t, span.clone())).collect(), vec![], parser)
            }
            _ => Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string())),
        }
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}