use crate::{
    error::{Error, Result},
    lexer::{Block, Token, Value},
    parser::{
        syntax_tree::{Expression, Literal},
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
        match tokens.next() {
            Some(Token::Value(Value::String(value))) => Ok(Expression::Call(
                "puts".to_string(),
                vec![Expression::Literal(Literal::String(value))],
            )),
            _ => Err(Error::syntax("Expected a string value as parameter".to_string(), 0).into()),
        }
    }
}
