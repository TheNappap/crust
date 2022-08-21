use itertools::Itertools;

use crate::{error::{Error, Result}, lexer::{Block, Token, Literal}, parser::{
        syntax_tree::{Expression},
        Parser, Type, self,
    }};

use super::BlockDefinition;

pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();
        match tokens.next() {
            Some(Token::Ident(id)) => match tokens.next() {
                Some(Token::Symbol('=')) => {
                    let tokens = tokens.collect_vec();
                    match tokens.len() {
                        0 => Err(Error::syntax("Expected a value in assignment".to_string(), 0).into()),
                        1 => match tokens.first() {
                            Some(Token::Literal(Literal::Int(i))) => Ok( Expression::Let(id, Box::new(Expression::Literal(parser::Literal::Int(*i))), Type::Int) ),
                            Some(Token::Literal(Literal::Float(f))) => Ok( Expression::Let(id, Box::new(Expression::Literal(parser::Literal::Float(*f))), Type::Float) ),
                            Some(Token::Literal(Literal::String(s))) => Ok( Expression::Let(id, Box::new(Expression::Literal(parser::Literal::String(s.clone()))), Type::String) ),
                            Some(Token::Ident(name)) => Ok( Expression::Let(id, Box::new(Expression::Symbol(name.clone(), Type::Inferred)), Type::Inferred) ),
                            _ => Err(Error::syntax("Expected a value in assignment".to_string(), 0).into()),
                        }
                        _ => {
                            let blocks: Vec<_> = parser.parse_tokens(tokens).collect::<Result<_>>()?;
                            match blocks.len() {
                                1 => {
                                    let expr = parser.parse_block(blocks[0].clone())?;
                                    Ok( Expression::Let(id, Box::new(expr), Type::Inferred) )
                                }
                                0 => Err(Error::syntax("Expected a block".to_string(), 0).into()),
                                _ => Err(Error::syntax("Expected only one block".to_string(), 0).into()),
                            }
                        }
                    }
                    
                }
                _ => Err(Error::syntax("Expected '='".to_string(), 0).into()),
            },
            _ => {
                Err(Error::syntax("Expected an identifier as variable name".to_string(), 0).into())
            }
        }
    }
}
