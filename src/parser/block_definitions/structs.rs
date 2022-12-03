use itertools::Itertools;

use crate::{
    error::{Result, Error},
    lexer::{Token},
    parser::{
        syntax_tree::{Expression, field_map::FieldMap},
        Parser, Type, Data
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Struct;

impl BlockDefinition for Struct {
    fn id(&self) -> &str {
        "struct"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(header.len() == 1);
        let Some(Token::Ident(name)) = header.first() else {
            return Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0));
        };

        let types = parser.parse_list(body).into_iter()
            .map(|tokens| parser.parse_parameter(tokens))
            .try_collect()?;

        let data = Data::new(name.to_string(), Type::Struct(types));
        Ok(Expression::Struct(data))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct New;

impl BlockDefinition for New {
    fn id(&self) -> &str {
        "new"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(header.len() == 1);
        let Some(Token::Ident(name)) = header.first() else {
            return Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0));
        };

        let var_iter = parser.parse_list(body).into_iter()
            .map(|tokens| parser.parse_param_expression(tokens).map(|(n,e)|((n,Type::Inferred),e)));
        let (types, exprs) = 
            itertools::process_results(var_iter, 
                |iter| iter.unzip::<_, _, FieldMap<_,_>, Vec<_>>())?;

        let data = Data::new(name.to_string(), Type::Struct(types));
        Ok(Expression::New(data, exprs))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}