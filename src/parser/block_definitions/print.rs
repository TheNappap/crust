use crate::{
    error::{Result, Error},
    lexer::{Block, Literal, Token},
    parser::{
        syntax_tree::{Expression, Type},
        Parser, Signature
    },
};

use super::BlockDefinition;


#[derive(Default)]
pub struct Print;

impl BlockDefinition for Print {
    fn id(&self) -> &str {
        "print"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        parse_print(header, body, parser, None)
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct PrintLn;

impl BlockDefinition for PrintLn {
    fn id(&self) -> &str {
        "println"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Block>, parser: &Parser) -> Result<Expression> {
        parse_print(header, body, parser, Some("\n".into()))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

fn parse_print(header: Vec<Token>, body: Vec<Block>, parser: &Parser, add: Option<String>) -> Result<Expression> {
    assert!(body.is_empty());
    let params = parser.parse_list(header)
                                    .contents.into_iter()
                                    .map(|tokens| parser.parse_expression(tokens))
                                    .collect::<Result<Vec<_>>>()?;
    
    let string_expr = if let Some(add_str) = add {
        Expression::Add(Box::new(params[0].clone()), Box::new(Expression::Literal(Literal::String(add_str))), Type::String)
    } else { params[0].clone() };

    let args_expr = if params.len() > 1 {
        let exprs = params.into_iter().skip(1).collect();
        Expression::AddrOf(exprs)
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
