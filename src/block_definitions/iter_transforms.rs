
use std::sync::atomic::{AtomicU32, Ordering};

use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{BlockTag, Expression, ExpressionKind, Fn, IterTransform, Parser, Signature, TransformKind, Type}, utils::{Result, ThrowablePosition}};

use super::BlockDefinition;

static TRANSFORM_FN_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Default)]
pub struct Map;

impl BlockDefinition for Map {
    fn id(&self) -> BlockTag {
        BlockTag::from("map")
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpectedly no input, block needs input".into())
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, mut input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let ExpressionKind::Iter(_, iter_transforms, _) = &mut input.kind else {
            return span.syntax("Expected iter as block input".into())
        };

        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(symbol) => symbol,
            _ => return span.syntax("Expected symbol for map variable".into())
        };
        let body = parser.iter_statement(body)
                                            .map_ok(Expression::return_if_forward)
                                            .try_collect()?;
        let id = TRANSFORM_FN_ID.load(Ordering::Relaxed);
        TRANSFORM_FN_ID.store(id+1, Ordering::Relaxed);
        let path = format!("__map_function{}", id).into();
        let signature = Signature::new(None, path, vec![symbol.ty], Type::Inferred);

        let transform = IterTransform {
            kind: TransformKind::Map,
            fun: Fn::new(signature, vec![symbol.name], body),
            span: span.clone()
        };
        iter_transforms.push(transform);

        Ok(input.kind)
    }
}


#[derive(Default)]
pub struct Filter;

impl BlockDefinition for Filter {
    fn id(&self) -> BlockTag {
        BlockTag::from("filter")
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        span.syntax("Unexpectedly no input, block needs input".into())
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, mut input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let ExpressionKind::Iter(_, iter_transforms, _) = &mut input.kind else {
            return span.syntax("Expected iter as block input".into())
        };

        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(symbol) => symbol,
            _ => return span.syntax("Expected symbol for filter variable".into())
        };
        let body = parser.iter_statement(body)
                                    .map_ok(Expression::return_if_forward)
                                    .try_collect()?;
        let id = TRANSFORM_FN_ID.load(Ordering::Relaxed);
        TRANSFORM_FN_ID.store(id+1, Ordering::Relaxed);
        let path = format!("__filter_function{}", id).into();
        let signature = Signature::new(None, path, vec![symbol.ty], Type::Bool);

        let transform = IterTransform{
            kind: TransformKind::Filter,
            fun: Fn::new(signature, vec![symbol.name], body),
            span: span.clone()
        };
        iter_transforms.push(transform);

        Ok(input.kind)
    }
}