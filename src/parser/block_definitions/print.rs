use crate::{
    error::{Error, Result},
    lexer::{Block, Token, Value},
    parser::{
        syntax_tree::{Expression, Literal, Type},
        Parser,
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
        let mut tokens = block.header.into_iter();
        let (string, args) = match tokens.next() {
            Some(Token::Value(Value::String(value))) => { 
                (Expression::Literal(Literal::String(value+"\n")), Expression::Literal(Literal::Int(0)))
            }
            Some(Token::Value(Value::Int(i))) => { 
                (Expression::Literal(Literal::String("%i\n".into())), Expression::Pointer(Literal::Int(i)))
            }
            Some(Token::Ident(name)) => { 
                (Expression::Symbol(name, Type::String), Expression::Pointer(Literal::Int(0)))
            }
            _ => return Err(Error::syntax("Expected a string value as parameter".to_string(), 0).into()),
        };

        Ok(Expression::Call(
            "__stdio_common_vfprintf".to_string(),
            vec![
                Expression::Literal(Literal::Int(0)),
                Expression::Call("__acrt_iob_func".to_string(), vec![Expression::Literal(Literal::Int(1))], vec![Expression::Literal(Literal::Int(0))]),
                string, Expression::Literal(Literal::Int(0)), args,
            ],
            vec![],
        ))
    }
}
