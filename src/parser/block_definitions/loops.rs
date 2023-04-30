

use crate::{lexer::{Token, Span}, parser::{Parser, Expression, ExpressionKind}, error::{Result, ThrowablePosition, ErrorKind}};

use super::BlockDefinition;


#[derive(Default)]
pub struct While;

impl BlockDefinition for While {
    fn id(&self) -> &str {
        "while"
    }

    fn parse(&self, _: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let condition = parser.parse_expression(header)?;
        let body = parser.parse_group(body)?;
        Ok(ExpressionKind::While(Box::new(condition), body))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct For;

impl BlockDefinition for For {
    fn id(&self) -> &str {
        "for"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpectedly no input, block needs input".to_string()))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let (var_name, var_type) = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(s, t) => (s,t),
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for loop variable".to_string()))
        };
        let body = parser.parse_group(body)?;
        Ok(ExpressionKind::For(Box::new(input), var_name.to_owned(), var_type.to_owned(), body))
    }
}