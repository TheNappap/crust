use itertools::Itertools;

use crate::{
    error::{Result, Error},
    lexer::{Token, Operator},
    parser::{
        syntax_tree::{Expression},
        Parser, Type
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

        let data = Type::Struct(name.to_owned(), types);
        Ok(Expression::Data(data))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Enum;

impl BlockDefinition for Enum {
    fn id(&self) -> &str {
        "enum"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(header.len() == 1);
        let Some(Token::Ident(name)) = header.first() else {
            return Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0));
        };

        let variants = parser.parse_list(body).into_iter().enumerate()
            .map(|(i, tokens)| match &tokens[0] {
                Token::Ident(variant) => Ok((variant.clone(), i)),
                _ => return Err(Error::syntax("Unexpected token as enum variant".to_string(), 0)),
            })
            .try_collect()?;

        let data = Type::Enum(name.to_owned(), variants);
        Ok(Expression::Data(data))
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
        let names: Vec<_> = header.iter().filter_map(|token|
            match token {
                Token::Ident(name) => Some(Ok(name)),
                Token::Operator(Operator::ColonColon) => None,
                _ => Some(Err(Error::syntax("Failed to parse data structure name".to_string(), 0))),
            })
            .try_collect()?;

        let exprs = if body.is_empty() {
            let data = Type::Named(names[1].to_owned());
            vec![Expression::Data(data)]
        } else {
            parser.parse_list(body).into_iter()
                .map(|tokens| parser.parse_param_expression(tokens).map(|t|t.1))
                .try_collect()?
        };

        let data = Type::Named(names[0].to_owned());
        Ok(Expression::New(data, exprs))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}

#[derive(Default)]
pub struct Field;

impl BlockDefinition for Field {
    fn id(&self) -> &str {
        "field"
    }

    fn parse(&self, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        assert!(body.is_empty());
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(Error::syntax("Field expression needs exactly 2 operands".to_string(), 0));
        }

        let operands: Vec<_> = token_list.into_iter()
            .map(|tokens| parser.parse_expression(tokens) )
            .try_collect()?;
        let Expression::Symbol(field_name, _) = operands[1].clone() else {
            return Err(Error::syntax("Field expression expected symbol as field name".to_string(), 0));
        };
        Ok(Expression::Field(Box::new(operands[0].clone()), field_name, Type::Inferred, -1))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}