use itertools::Itertools;

use crate::{lexer::{Token, Operator, Span, TokenKind}, parser::{Parser, Expression, syntax_tree::patterns::Pattern, Type, ExpressionKind}, error::{Result, ThrowablePosition, ErrorKind}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Match;

impl BlockDefinition for Match {
    fn id(&self) -> &str {
        "match"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let match_value = parser.parse_expression(header)?;
        let cases = parser.parse_group(body)?
            .into_iter()
            .map(|expr| match expr.kind {
                ExpressionKind::Case(pattern, exprs) => Ok((pattern.clone(), exprs.clone())),
                _ => Err(span.error(ErrorKind::Syntax, "Expected case expression in match expression".to_string()))
            })
            .try_collect()?;

        Ok(ExpressionKind::Match(Box::new(match_value), Type::Inferred, cases))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Case;

impl BlockDefinition for Case {
    fn id(&self) -> &str {
        "case"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let pattern = match header.iter().map(|t|&t.kind).collect_vec().as_slice() {
            [TokenKind::Underscore] => Pattern::Ident("_".to_owned()),
            [TokenKind::Ident(name)] => Pattern::Ident(name.to_owned()),
            [TokenKind::Ident(ty), TokenKind::Operator(Operator::ColonColon), TokenKind::Ident(name)] => Pattern::EnumVariant(ty.to_owned(), name.to_owned()),
            _ => return Err(span.error(ErrorKind::Syntax, "Failed to parse pattern in match expression".to_string())),
        };

        let exprs = parser.parse_group(body)?;
        Ok(ExpressionKind::Case(pattern, exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
