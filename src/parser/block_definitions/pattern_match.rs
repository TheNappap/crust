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
        let names: Vec<_> = header.iter().filter_map(|token|
            match token {
                Token::Ident(name) => Some(Ok(name)),
                Token::Operator(Operator::ColonColon) => None,
                _ => Some(Err(Error::syntax("Failed to parse data structure name".to_string(), 0))),
            })
            .try_collect()?;
        let pattern = Pattern::new(names[0].to_owned(), names[1].to_owned());

        let exprs = parser.parse_group(body)?;
        Ok(Expression::Case(pattern, exprs))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
