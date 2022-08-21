use crate::{
    error::{Error, Result},
    lexer::{Block, Delimeter, Token, Literal},
    parser::{
        syntax_tree::{Expression},
        Parser, self
    },
};

use super::BlockDefinition;

pub struct Call;

impl BlockDefinition for Call {
    fn id(&self) -> &str {
        "call"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Group(Delimeter::Parens, tokens)) => {
                    let exprs = tokens
                        .into_iter()
                        .map(|token| match token {
                            Token::Literal(Literal::String(value)) => {
                                Ok(Expression::Literal(parser::Literal::String(value.clone())))
                            }
                            _ => Err(Error::syntax(
                                "Expected a string value as parameter".to_string(),
                                0,
                            )
                            .into()),
                        })
                        .collect::<Result<_>>()?;
                    Ok(Expression::Call(id, exprs, vec![]))
                }
                _ => Err(Error::syntax("Expected a parameter group".to_string(), 0).into()),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as function name".to_string(), 0).into())
            }
        }
    }
}
