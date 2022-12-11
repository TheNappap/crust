
use crate::{lexer::{Token, Delimeter, Operator}, parser::{Parser, Expression, block_definitions::call::Call}, error::{Error, Result}};

use super::{BlockDefinition, data::Field};

#[derive(Default)]
pub struct Dot;

impl BlockDefinition for Dot {
    fn id(&self) -> &str {
        "dot"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(body.len() == 0);
        let token_list = parser.parse_list(header.clone());
        if token_list.len() != 2 {
            return Err(Error::syntax("Binary operator expects exactly 2 operands".to_string(), 0));
        }

        match &token_list[1][..] {
            [Token::Ident(_)] => Field.parse(header, vec![], parser),
            [Token::Ident(name), Token::Group(Delimeter::Parens, tokens)] => {
                let mut params = token_list[0].clone();
                params.extend(tokens.clone());
                let mut header = vec![Token::Ident("_".to_owned()), Token::Operator(Operator::ColonColon), Token::Ident(name.to_owned())];
                header.push(Token::Group(Delimeter::Parens, params));
                Call.parse(header, vec![], parser)
            }
            _ => Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0)),
        }
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}