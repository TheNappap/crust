use itertools::Itertools;

use crate::{
    error::{Result, ErrorKind, ThrowablePosition},
    lexer::{Literal, Token, Span},
    parser::{
        syntax_tree::{Expression},
        Parser, Signature, BinOpKind, Type, ExpressionKind
    },
};

use super::BlockDefinition;


#[derive(Default)]
pub struct Print;

impl BlockDefinition for Print {
    fn id(&self) -> &str {
        "print"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        parse_print(span, header, body, parser, None)
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct PrintLn;

impl BlockDefinition for PrintLn {
    fn id(&self) -> &str {
        "println"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        parse_print(span, header, body, parser, Some("\n".into()))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

fn parse_print(span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser, add: Option<String>) -> Result<ExpressionKind> {
    assert!(body.is_empty());
    let params: Vec<_> = parser.parse_list(header)
                                    .into_iter()
                                    .map(|tokens| parser.parse_expression(tokens))
                                    .try_collect()?;
    
    let string_expr = if params.is_empty() { Expression::new(ExpressionKind::Literal(Literal::String("".into())), (span.clone())) }
                                  else { params[0].clone() };
    
    let string_expr = if let Some(add_str) = add {
        let span = string_expr.span.to_owned();
        let add_token = Expression::new(ExpressionKind::Literal(Literal::String(add_str)), span.clone());
        let token = ExpressionKind::BinOp(BinOpKind::Add, Box::new(string_expr.clone()), Box::new(add_token), Type::String);
        Expression::new(token, span)
    } else { string_expr.clone() };

    let args_expr = if params.len() > 1 {
        let exprs = params.into_iter().skip(1).collect();
        ExpressionKind::AddrOf(exprs)
    } else {
        ExpressionKind::Literal(Literal::Int(0))
    };

    Ok(ExpressionKind::Call(
        Signature::new(None, "__stdio_common_vfprintf",vec![Type::Int,Type::Int,Type::String,Type::Int,Type::Int],Type::Void),
        vec![
            Expression::new(ExpressionKind::Literal(Literal::Int(0)), span.clone()),
            Expression::new(ExpressionKind::Call(Signature::new(None, "__acrt_iob_func", vec![Type::Int], Type::Int), vec![Expression::new(ExpressionKind::Literal(Literal::Int(1)), span.clone())]), span.clone()),
            string_expr, 
            Expression::new(ExpressionKind::Literal(Literal::Int(0)), span.clone()), 
            Expression::new(args_expr, span.clone()),
        ],
    ))
}
