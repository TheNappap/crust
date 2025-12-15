use crate::{
    error::{Result, ErrorKind, ThrowablePosition},
    lexer::{Token, Delimeter, Span, TokenKind},
    parser::{
        syntax_tree::{Expression},
        Parser, ExpressionKind
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Return;

impl BlockDefinition for Return {
    fn id(&self) -> &str {
        "return"
    }

    fn parse(&self, _: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let span = if let Some(token) = body.first() {
            body.iter().map(|t|&t.span).fold(token.span.clone(), |acc,s| acc.union(&s))
        } else {
            header.last().unwrap().span.clone()
        };
        let body_token = Token::new(TokenKind::Group(Delimeter::Braces, body), span);
        header.push(body_token);
        let expr = parser.parse_expression(header)?;
        Ok(ExpressionKind::Return(Box::new(expr)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Forward;

impl BlockDefinition for Forward {
    fn id(&self) -> &str {
        "forward"
    }

    fn parse(&self, _: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let span = if let Some(token) = body.first() {
            body.iter().map(|t|&t.span).fold(token.span.clone(), |acc,s| acc.union(&s))
        } else {
            header.last().unwrap().span.clone()
        };
        let body_token = Token::new(TokenKind::Group(Delimeter::Braces, body), span);
        header.push(body_token);
        
        let expr = parser.parse_expression(header)?;
        if let ExpressionKind::Return(_) | ExpressionKind::Forward(_) = expr.kind {
            return Ok(expr.kind);
        }
        Ok(ExpressionKind::Forward(Box::new(expr)))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}