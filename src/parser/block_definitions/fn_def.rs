use itertools::Itertools;

use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{Expression, ExpressionKind, Fn, Parser, Type, blocks::BlockTag, syntax_tree::fn_expr::Signature}, utils::{ErrorKind, Result, ThrowablePosition}
};

use super::BlockDefinition;

#[derive(Default)]
pub struct FnDef;

impl BlockDefinition for FnDef {
    fn id(&self) -> BlockTag {
        BlockTag::from("fn")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut tokens = header.into_iter();

        let name = match tokens.next().map(|t|t.kind.clone()) {
            Some(TokenKind::Ident(value)) => value,
            _ => {
                return Err(span.error(ErrorKind::Syntax, "Expected an identifier as function name".to_string()))
            }
        };

        let (param_names, param_types) : (Vec<String>, Vec<Type>) = match tokens.next().map(|t|t.kind.clone()) {
            Some(TokenKind::Group(Delimeter::Parens, params)) => {
                parser.iter_parameter(params.clone())
                    .map_ok(|(name, tokens)| -> Result<_> {
                        if name == "self" && tokens.len() == 0 {
                            Ok((name, Type::Inferred))
                        } else {
                            assert!(tokens.len() == 1);
                            Ok((name, Type::from(tokens[0].clone())?))
                        }
                    })
                    .flatten()
                    .process_results(|iter| iter.unzip())?
            }
            _ => return Err(span.error(ErrorKind::Syntax, "Expected parameters in parens".to_string())),
        };

        let returns = match (tokens.next().map(|t|t.kind.clone()), tokens.next()) {
            (None, None) => Type::Void,
            (Some(TokenKind::Arrow), Some(token)) => Type::from(token)?,
            _ => return Err(span.error(ErrorKind::Syntax, "Unexpected symbols after function header".to_string())),
        };

        let ty = if param_names.len() > 0 && param_names[0] == "self" {
            Some(Type::Inferred)
        } else {
            None
        };
        let signature = Signature::new(ty, name.into(), param_types, returns);

        if body.is_empty() {
            Ok(ExpressionKind::Signature(signature))
        } else {
            let mut body: Vec<_> = parser.iter_statement(body)
                                    .map_ok(Expression::return_if_forward)
                                    .try_collect()?;
            if let Some(Expression { kind, span, .. }) = body.last() && !matches!(kind, ExpressionKind::Return(_)) {
                body.push(Expression::new(ExpressionKind::Return(Box::new(Expression::new(ExpressionKind::Void, span.clone()))), span.clone()));
            }
            let fun = Fn::new(signature, param_names, body);
            Ok(ExpressionKind::Fn(fun))
        }
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Impl;

impl BlockDefinition for Impl {
    fn id(&self) -> BlockTag {
        BlockTag::from("impl")
    }

    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let header = header.iter().map(|t|&t.kind).collect_vec();
        let (type_name, trait_name) = match header.as_slice() {
            [TokenKind::Ident(name)] => (name.clone(), None),
            [TokenKind::Ident(trait_name), TokenKind::Ident(for_), TokenKind::Ident(name)]
                if for_ == "for" => (name.clone(), Some(trait_name.clone())),
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol as type name or trait".to_string())),
        };

        let fns = parser.iter_block(body)
            .map_ok(|exp| {
                if let ExpressionKind::Fn(mut fun) = exp.kind.clone() {
                    fun.set_self_type(&type_name);
                    Ok(fun) 
                }
                else { return Err(span.error(ErrorKind::Syntax, "Expected symbol as type name".to_string())); }
            })
            .flatten()
            .try_collect()?;
        Ok(ExpressionKind::Impl(type_name.to_owned(), fns, trait_name))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}
