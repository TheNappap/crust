

use itertools::Itertools;

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
        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(s) => s,
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for loop variable".to_string()))
        };
        let body = parser.parse_group(body)?;
        Ok(ExpressionKind::Fold(Box::new(input), symbol, None, body))
    }
}

#[derive(Default)]
pub struct Fold;

impl BlockDefinition for Fold {
    fn id(&self) -> &str {
        "fold"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpectedly no input, block needs input".to_string()))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let header: Vec<_> = parser.parse_list(header).into_iter()
                                                    .map(|tokens|{
                                                        parser.parse_expression(tokens)
                                                    })
                                                    .try_collect()?;
        let init_expression = header[0].clone();
        let acc_symbol = match &header[1].kind {
            ExpressionKind::Symbol(s) => s.clone(),
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for accumulator variable".to_string()))
        };
        let symbol = match &header[2].kind {
            ExpressionKind::Symbol(s) => s.clone(),
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for loop variable".to_string()))
        };
        let body = parser.parse_group(body)?;
        Ok(ExpressionKind::Fold(Box::new(input), symbol, Some((Box::new(init_expression), acc_symbol)), body))
    }
}