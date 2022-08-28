use crate::{
    error::{Error, Result},
    lexer::{Block, Delimeter, Token},
    parser::{
        syntax_tree::{Expression},
        Parser, Type, Signature
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Call;

impl BlockDefinition for Call {
    fn id(&self) -> &str {
        "call"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        let mut tokens = header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Group(Delimeter::Parens, tokens)) => {
                    let exprs = parser.parse_list(tokens)
                        .contents
                        .into_iter()
                        .map(|tokens| parser.parse_expression(tokens))
                        .collect::<Result<_>>()?;
                    Ok(Expression::Call(Signature::new(&id, vec![], Type::Inferred), exprs))
                }
                _ => Err(Error::syntax("Expected a parameter group".to_string(), 0)),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as function name".to_string(), 0))
            }
        }
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
