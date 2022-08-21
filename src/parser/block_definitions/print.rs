use crate::{
    error::{Error, Result},
    lexer::{Block, Token, Literal},
    parser::{
        syntax_tree::{Expression, Type},
        Parser, self
    },
};

use super::BlockDefinition;

pub struct Print;

impl BlockDefinition for Print {
    fn id(&self) -> &str {
        "print"
    }

    fn parse(&self, block: Block, _parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter().peekable();
        let string = get_string(&mut tokens)?;
        let args = get_args(&mut tokens)?;

        Ok(Expression::Call(
            "__stdio_common_vfprintf".to_string(),
            vec![
                Expression::Literal(parser::Literal::Int(0)),
                Expression::Call("__acrt_iob_func".to_string(), vec![Expression::Literal(parser::Literal::Int(1))], vec![Type::Int]),
                string, Expression::Literal(parser::Literal::Int(0)), args,
            ],
            vec![],
        ))
    }
}

fn get_string(tokens: &mut impl Iterator<Item = Token>) -> Result<Expression> {
    let str_expr = match tokens.next() {
        Some(Token::Literal(Literal::String(value))) => Expression::Literal(parser::Literal::String(value+"\n")),
        Some(Token::Ident(name)) => Expression::Symbol(name, Type::String),
        _ => return Err(Error::syntax("Expected a string value as parameter".to_string(), 0).into()),
    };
    Ok(str_expr)
}

fn get_args(tokens: &mut impl Iterator<Item = Token>) -> Result<Expression> {
    let args= match tokens.next() {
        Some(Token::Literal(Literal::String(value))) => Expression::AddrOf(Box::new(Expression::Literal(parser::Literal::String(value)))),
        Some(Token::Literal(Literal::Int(i))) => Expression::AddrOf(Box::new(Expression::Literal(parser::Literal::Int(i)))),
        Some(Token::Literal(Literal::Float(f))) => Expression::AddrOf(Box::new(Expression::Literal(parser::Literal::Float(f)))),
        Some(Token::Ident(name)) => Expression::AddrOf(Box::new(Expression::Symbol(name, Type::Inferred))), 
        None => Expression::Literal(parser::Literal::Int(0)),
        _ => return Err(Error::syntax("Unexpected token instead of parameter".to_string(), 0).into()),
    };
    Ok(args)
}