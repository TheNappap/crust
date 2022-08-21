use crate::{
    error::{Result},
    lexer::{Block, Literal},
    parser::{
        syntax_tree::{Expression, Type},
        Parser
    },
};

use super::BlockDefinition;

pub struct Print;

impl BlockDefinition for Print {
    fn id(&self) -> &str {
        "print"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        assert!(block.body.is_empty());
        let params = parser.parse_list(block.header)
                                        .contents.into_iter()
                                        .map(|tokens| parser.parse_expression(tokens))
                                        .collect::<Result<Vec<_>>>()?;

        Ok(Expression::Call(
            "__stdio_common_vfprintf".to_string(),
            vec![
                Expression::Literal(Literal::Int(0)),
                Expression::Call("__acrt_iob_func".to_string(), vec![Expression::Literal(Literal::Int(1))], vec![Type::Int]),
                params[0].clone(), Expression::Literal(Literal::Int(0)), 
                if params.len() > 1 { 
                    Expression::AddrOf(Box::new(params[1].clone()))
                } else {
                    Expression::Literal(Literal::Int(0))
                },
            ],
            vec![],
        ))
    }
}