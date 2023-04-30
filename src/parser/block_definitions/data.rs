use itertools::Itertools;

use crate::{
    error::{Result, ThrowablePosition, ErrorKind},
    lexer::{Token, Operator, Span, TokenKind},
    parser::{
        syntax_tree::{Expression},
        Parser, Type, ExpressionKind
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Struct;

impl BlockDefinition for Struct {
    fn id(&self) -> &str {
        "struct"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()));
        };

        let types = parser.parse_list(body).into_iter()
            .map(|tokens| parser.parse_parameter(tokens))
            .try_collect()?;

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
    fn id(&self) -> &str {
        "enum"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(header.len() == 1);
        let Some(TokenKind::Ident(name)) = header.first().map(|t|&t.kind) else {
            return Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()));
        };

        let variants = parser.parse_list(body).into_iter().enumerate()
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
    fn id(&self) -> &str {
        "new"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let names: Vec<_> = header.iter().filter_map(|token|
            match &token.kind {
                TokenKind::Ident(name) => Some(Ok(name)),
                TokenKind::Operator(Operator::ColonColon) => None,
                _ => Some(Err(span.error(ErrorKind::Syntax, "Failed to parse data structure name".to_string()))),
            })
            .try_collect()?;

        let exprs = if body.is_empty() {
            let data = ExpressionKind::Data(Type::Named(names[1].to_owned()));
            let pos = header.last().unwrap().span.clone().end();
            let span = Span::new(pos.clone(), pos);
            vec![Expression::new(data, span)]
        } else {
            parser.parse_list(body).into_iter()
                .map(|tokens| parser.parse_param_expression(tokens).map(|t|t.1))
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
    fn id(&self) -> &str {
        "field"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let token_list = parser.parse_list(header);
        if token_list.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Field expression needs exactly 2 operands".to_string()));
        }

        let operands: Vec<_> = token_list.into_iter()
            .map(|tokens| parser.parse_expression(tokens) )
            .try_collect()?;
        let ExpressionKind::Symbol(field_name, _) = &operands[1].kind else {
            return Err(span.error(ErrorKind::Syntax, "Field expression expected symbol as field name".to_string()));
        };
        Ok(ExpressionKind::Field(Box::new(operands[0].clone()), field_name.to_owned(), Type::Inferred, -1))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}