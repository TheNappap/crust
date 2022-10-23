use itertools::{Itertools, process_results};

use crate::{
    error::{Error, Result},
    lexer::{Block, Delimeter, Token, Operator},
    parser::{Expression, Fn, Parser, Type, syntax_tree::fn_expr::Signature},
};

use super::BlockDefinition;

#[derive(Default)]
pub struct FnDef;

impl BlockDefinition for FnDef {
    fn id(&self) -> &str {
        "fn"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Block>, parser: &Parser) -> Result<Expression> {
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
        .contents.into_iter()
        .map(|tokens|{
            let mut tokens = tokens.into_iter();
            let name = match tokens.next() {
                Some(Token::Ident(name)) => name,
                _ => return Err(Error::syntax("Expected an identifier as parameter name".to_string(), 0))
            };

            if !matches!(tokens.next(), Some(Token::Operator(Operator::Colon))) {  
                return Err(Error::syntax("Expected an ':' and a type name".to_string(), 0))
            }
            let ty = match tokens.next() {
                Some(token) => Type::from(token),
                _ => return Err(Error::syntax("Expected an identifier as type name".to_string(), 0))
            }; 
            Ok((name, ty))
        });
        
        let (param_names, param_types) = process_results(params, |iter| iter.unzip())?;

        let returns = match (tokens.next(), tokens.next()) {
            (None, None) => Type::Void,
            (Some(Token::Operator(Operator::Arrow)), Some(token)) => token.into(),
            _ => return Err(Error::syntax("Unexpected symbols after function header".to_string(), 0)),
        };

        let signature = Signature::new(&name, param_types, returns);
        let exprs = body
            .into_iter()
            .map(|block| parser.parse_block_expression(block))
            .try_collect()?;
        Ok(Expression::Fn(Fn::new(signature, param_names, exprs)))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Block>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
