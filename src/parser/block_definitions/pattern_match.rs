use itertools::Itertools;

use crate::{lexer::{Token, Operator}, parser::{Parser, Expression, syntax_tree::patterns::Pattern, Type}, error::{Result, Error}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Match;

impl BlockDefinition for Match {
    fn id(&self) -> &str {
        "match"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let match_value = parser.parse_expression(header)?;
        let cases = parser.parse_group(body)?
            .into_iter()
            .map(|expr| match expr {
                Expression::Case(pattern, exprs) => Ok((pattern, exprs)),
                _ => Err(Error::syntax("Expected case expression in match expression".to_string(), 0))
            })
            .try_collect()?;

        Ok(Expression::Match(Box::new(match_value), Type::Inferred, cases))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Case;

impl BlockDefinition for Case {
    fn id(&self) -> &str {
        "case"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let pattern = match header.as_slice() {
            [Token::Underscore] => Pattern::Ident("_".to_owned()),
            [Token::Ident(name)] => Pattern::Ident(name.to_owned()),
            [Token::Ident(ty), Token::Operator(Operator::ColonColon), Token::Ident(name)] => Pattern::EnumVariant(ty.to_owned(), name.to_owned()),
            _ => return Err(Error::syntax("Failed to parse pattern in match expression".to_string(), 0)),
        };

        let exprs = parser.parse_group(body)?;
        Ok(Expression::Case(pattern, exprs))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
