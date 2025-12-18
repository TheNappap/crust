use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{
        ExpressionKind, Parser, blocks::BlockTag, syntax_tree::Expression
    }, utils::{ErrorKind, Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Return;

impl BlockDefinition for Return {
    fn id(&self) -> BlockTag {
        BlockTag::from("return")
    }

    fn parse(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        if !body.is_empty() {
            header.push(Token::new(TokenKind::Group(Delimeter::Braces, body), span.clone()));
        }
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
    fn id(&self) -> BlockTag {
        BlockTag::from("forward")
    }

    fn parse(&self, span: &Span, _header: Vec<Token>, _body: Vec<Token>, _parser: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "This block should not be parsed through `parse_expression()`".to_string()))
    }

    fn parse_expression(&self, span: Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        if !body.is_empty() {
            header.push(Token::new(TokenKind::Group(Delimeter::Braces, body), span));
        }
        let expr = parser.parse_expression(header)?;
        Ok(expr.forward())
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}