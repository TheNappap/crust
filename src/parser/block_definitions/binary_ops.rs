use crate::{lexer::{Block, Token, Literal}, parser::{Parser, Expression, Type, self}, error::{Result, Error}};

use super::BlockDefinition;

pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> &str {
        "add"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();
        Ok(Expression::Add(Box::new(get_param(&mut tokens)?), Box::new(get_param(&mut tokens)?), Type::Inferred))
    }
}

fn get_param(tokens: &mut impl Iterator<Item = Token>) -> Result<Expression> {
    let args= match tokens.next() {
        Some(Token::Literal(Literal::Int(i))) => Expression::Literal(parser::Literal::Int(i)),
        Some(Token::Literal(Literal::Float(f))) => Expression::Literal(parser::Literal::Float(f)),
        Some(Token::Literal(Literal::String(value))) => Expression::Literal(parser::Literal::String(value)),
        Some(Token::Ident(name)) => Expression::Symbol(name, Type::Inferred),
        a => return Err(Error::syntax(format!("Unexpected token instead of parameter {:?}", a), 0).into()),
    };
    Ok(args)
}