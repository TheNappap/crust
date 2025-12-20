use itertools::Itertools;

use crate::{
    lexer::{Span, Token, TokenKind}, parser::{
        ExpressionKind, OrderedMap, Parser, Type, BlockTag, Expression
    }, utils::{ErrorKind, Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Struct;

impl BlockDefinition for Struct {
    fn id(&self) -> BlockTag {
        BlockTag::from("struct")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return Err(span.error(ErrorKind::Syntax, "Expected symbol as type name".to_string()));
        };

        let types : OrderedMap<_,_> = parser.iter_parameter(body)
                                .map_ok(|(name, tokens)| -> Result<_> {
                                    Ok((name, Type::from(tokens[0].clone())?))
                                })
                                .flatten()
                                .try_collect()?;

        let offsets: Vec<i32> = types.values()
            .scan(0, |acc, t| {
                *acc = *acc + t.size() as i32;
                Some(*acc)
            })
            .collect();
        let types = types.iter()
            .enumerate()
            .map(|(i, (s,t))| 
                if i == 0 { (s.clone(), (t.clone(),0)) }
                else { (s.clone(), (t.clone(), offsets[i-1])) }
            ).collect();

        let data = Type::Struct(name.to_owned(), types);
        Ok(ExpressionKind::Data(data))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Enum;

impl BlockDefinition for Enum {
    fn id(&self) -> BlockTag {
        BlockTag::from("enum")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return Err(span.error(ErrorKind::Syntax, "Expected identifier as enum name".to_string()));
        };

        let variants = parser.split_list(body).enumerate()
            .map(|(i, tokens)| match &tokens[0].kind {
                TokenKind::Ident(variant) => Ok((variant.clone(), i)),
                _ => return Err(span.error(ErrorKind::Syntax, "Unexpected token as enum variant".to_string())),
            })
            .try_collect()?;

        let data = Type::Enum(name.to_owned(), variants);
        Ok(ExpressionKind::Data(data))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct New;

impl BlockDefinition for New {
    fn id(&self) -> BlockTag {
        BlockTag::from("new")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let names: Vec<_> = header.iter().filter_map(|token|
            match &token.kind {
                TokenKind::Ident(name) => Some(Ok(name)),
                TokenKind::ColonColon => None,
                _ => Some(Err(span.error(ErrorKind::Syntax, "Failed to parse data structure name".to_string()))),
            })
            .try_collect()?;

        let exprs = if body.is_empty() {
            let data = ExpressionKind::Data(Type::Named(names[1].to_owned()));
            let pos = header.last().unwrap().span.clone().end();
            let span = Span::new(pos.clone(), pos);
            vec![Expression::new(data, span)]
        } else {
            parser.iter_parameter(body).into_iter()
                .map_ok(|(_,t)| parser.parse_expression(t))
                .flatten()
                .try_collect()?
        };

        let data = Type::Named(names[0].to_owned());
        Ok(ExpressionKind::New(data, exprs))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Field;

impl BlockDefinition for Field {
    fn id(&self) -> BlockTag {
        BlockTag::from("field")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Field expression expected 2 operands".to_string()));
        }
            
        let ExpressionKind::Symbol(field_symbol) = &operands[1].kind else {
            return Err(span.error(ErrorKind::Syntax, "Field expression expected symbol as field name".to_string()));
        };
        assert_eq!(field_symbol.ty, Type::Inferred);
        Ok(ExpressionKind::Field(Box::new(operands[0].clone()), field_symbol.to_owned(), -1))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let field = parser.parse_expression(header)?;

        let ExpressionKind::Symbol(field_symbol) = field.kind else {
            return Err(span.error(ErrorKind::Syntax, "Field expression expected symbol as field name".to_string()));
        };
        assert_eq!(field_symbol.ty, Type::Inferred);
        Ok(ExpressionKind::Field(Box::new(input), field_symbol.to_owned(), -1))
    }
}