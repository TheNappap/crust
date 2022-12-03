use itertools::{process_results, Itertools};

use crate::{
    error::{Error, Result},
    lexer::{Delimeter, Token, Operator},
    parser::{Expression, Fn, Parser, Type, syntax_tree::fn_expr::Signature},
};

use super::BlockDefinition;

#[derive(Default)]
pub struct FnDef;

impl BlockDefinition for FnDef {
    fn id(&self) -> &str {
        "fn"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        let mut tokens = header.into_iter();

        let name = match tokens.next() {
            Some(Token::Ident(value)) => value,
            _ => {
                return Err(
                    Error::syntax("Expected an identifier as function name".to_string(), 0),
                )
            }
        };

        let params = match tokens.next() {
            Some(Token::Group(Delimeter::Parens, params)) => parser.parse_list(params),
            _ => return Err(Error::syntax("Expected parameters in parens".to_string(), 0)),
        }
        .into_iter()
        .map(|tokens| parser.parse_parameter(tokens));
        
        let (param_names, param_types) = process_results(params, |iter| iter.unzip())?;

        let returns = match (tokens.next(), tokens.next()) {
            (None, None) => Type::Void,
            (Some(Token::Operator(Operator::Arrow)), Some(token)) => Type::from(token)?,
            _ => return Err(Error::syntax("Unexpected symbols after function header".to_string(), 0)),
        };

        let signature = Signature::new(&name, param_types, returns);
        let exprs = parser.parse_group(body)?;
        Ok(Expression::Fn(Fn::new(signature, param_names, exprs)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}


#[derive(Default)]
pub struct Impl;

impl BlockDefinition for Impl {
    fn id(&self) -> &str {
        "impl"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(header.len() == 1);

        let Expression::Symbol(name, _) = parser.parse_expression(header)? else {
            return Err(Error::syntax("Expected symbol as type name".to_string(), 0));
        };
        let fns = parser.parse_group(body)?.into_iter()
            .map(|exp| 
                if let Expression::Fn(mut fun) = exp {
                    fun.add_type_name(name.clone());
                    Ok(fun) 
                }
                else { return Err(Error::syntax("Expected symbol as type name".to_string(), 0)); } 
            )
            .try_collect()?;
        Ok(Expression::Impl(name, fns))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
