use crate::{
    error::{Result, Error},
    lexer::{Token, Delimeter},
    parser::{
        syntax_tree::{Expression},
        Parser
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Return;

impl BlockDefinition for Return {
    fn id(&self) -> &str {
        "return"
    }

    fn parse(&self, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        header.push(Token::Group(Delimeter::Braces, body));
        let expr = parser.parse_expression(header)?;
        Ok(Expression::Return(Box::new(expr)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}