use crate::{
    error::{Result},
    lexer::{Block},
    parser::{
        syntax_tree::{Expression},
        Parser
    },
};

use super::BlockDefinition;

pub struct Return;

impl BlockDefinition for Return {
    fn id(&self) -> &str {
        "return"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let expr = parser.parse_expression(block.header)?;
        Ok(Expression::Return(Box::new(expr)))
    }
}