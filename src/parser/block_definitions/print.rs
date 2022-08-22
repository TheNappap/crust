use crate::{
    error::{Result},
    lexer::{Block, Literal},
    parser::{
        syntax_tree::{Expression, Type},
        Parser, Signature
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
        parse_print(block, parser, None)
    }
}

pub struct PrintLn;

impl BlockDefinition for PrintLn {
    fn id(&self) -> &str {
        "println"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        parse_print(block, parser, Some("\n".into()))
    }
}

fn parse_print(block: Block, parser: &Parser, add: Option<String>) -> Result<Expression> {
    assert!(block.body.is_empty());
    let params = parser.parse_list(block.header)
                                    .contents.into_iter()
                                    .map(|tokens| parser.parse_expression(tokens))
                                    .collect::<Result<Vec<_>>>()?;
    
    let string_expr = if let Some(add_str) = add {
        Expression::Add(Box::new(params[0].clone()), Box::new(Expression::Literal(Literal::String(add_str))), Type::String)
    } else { params[0].clone() };

    let args_expr = if params.len() > 1 { 
        Expression::AddrOf(Box::new(params[1].clone()))
    } else {
        Expression::Literal(Literal::Int(0))
    };

    Ok(Expression::Call(
        Signature::new("__stdio_common_vfprintf",vec![Type::Int,Type::Int,Type::String,Type::Int,Type::Int],Type::Void),
        vec![
            Expression::Literal(Literal::Int(0)),
            Expression::Call(Signature::new("__acrt_iob_func", vec![Type::Int], Type::Int), vec![Expression::Literal(Literal::Int(1))]),
            string_expr, 
            Expression::Literal(Literal::Int(0)), 
            args_expr,
        ],
    ))
}
