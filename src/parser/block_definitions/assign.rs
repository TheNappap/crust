

use crate::{error::{Result, ErrorKind, ThrowablePosition}, lexer::{Token, Span}, parser::{
        syntax_tree::{Expression},
        Parser, ExpressionKind,
    }};

use super::BlockDefinition;


#[derive(Default)]
pub struct Let;

impl BlockDefinition for Let {
    fn id(&self) -> &str {
        "let"
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let name_expr = parser.parse_expression(header)?;
        let (id, ty) = match name_expr.kind {
            ExpressionKind::Symbol(id, ty) => (id, ty),
            _ => {
                return Err(name_expr.span.error(ErrorKind::Syntax, "Expected an identifier as variable name".to_string()));
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(ExpressionKind::Let(id.clone(), Box::new(value), ty.clone()))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Mut;

impl BlockDefinition for Mut {
    fn id(&self) -> &str {
        "mut"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let name_expr = parser.parse_expression(header)?;
        let id = match name_expr.kind {
            ExpressionKind::Symbol(id, _) => id,
            _ => {
                return Err(span.error(ErrorKind::Syntax, "Expected an identifier as variable name".to_string()));
            }
        };
        let value = parser.parse_expression(body)?;
        Ok(ExpressionKind::Mut(id.clone(), Box::new(value)))
    }

    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
