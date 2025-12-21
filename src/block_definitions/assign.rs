

use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{
        ExpressionKind, Parser, BlockTag, Expression
    }, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> BlockTag {
        BlockTag::from("let")
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let name_expr = parser.parse_expression(header)?;
        let symbol = match name_expr.kind {
            ExpressionKind::Symbol(symbol) => symbol,
            _ => {
                return name_expr.span.syntax("Expected an identifier as variable name".to_string());
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(ExpressionKind::Let(symbol.clone(), Box::new(value)))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}


#[derive(Default)]
pub struct Mut;

impl BlockDefinition for Mut {
    fn id(&self) -> BlockTag {
        BlockTag::from("mut")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        let (id, field) = match &operands[0].kind {
            ExpressionKind::Symbol(s) => (s, None),
            ExpressionKind::Field(expr, field_symbol, offset) => 
                match &expr.kind {
                    ExpressionKind::Symbol(symbol) => (symbol, Some((field_symbol.clone(), *offset))),
                    k => {
                        return span.syntax(format!("Expected an identifier as variable name, got {:?}", k));
                    }
                },
            k => {
                return span.syntax(format!("Expected an identifier as variable name, got {:?}", k));
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(ExpressionKind::Mut(id.clone(), field, Box::new(value)))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpected input, block doesn't handle input".into())
    }
}
