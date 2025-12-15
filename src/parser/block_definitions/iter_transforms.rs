
use std::sync::atomic::{AtomicU32, Ordering};

use itertools::Itertools;

use crate::{utils::{ErrorKind, Result, ThrowablePosition}, lexer::{Span, Token}, parser::{Expression, ExpressionKind, Fn, Parser, Signature, Type, syntax_tree::expression::{IterTransform, TransformKind}}};

use super::BlockDefinition;

static TRANSFORM_FN_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Default)]
pub struct Map;

impl BlockDefinition for Map {
    fn id(&self) -> &str {
        "map"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpectedly no input, block needs input".to_string()))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, mut input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let ExpressionKind::Iter(_, iter_transforms, _) = &mut input.kind else {
            return Err(span.error(ErrorKind::Syntax, "Expected iter as block input".to_string()))
        };

        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(symbol) => symbol,
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for map variable".to_string()))
        };
        let body = parser.iter_statement(body)
                                            .map_ok(Expression::return_if_forward)
                                            .try_collect()?;
        let id = TRANSFORM_FN_ID.load(Ordering::Relaxed);
        TRANSFORM_FN_ID.store(id+1, Ordering::Relaxed);
        let name = format!("__map_function{}", id);
        let signature = Signature::new(None, &name, vec![symbol.ty], Type::Inferred);

        let transform = IterTransform{
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
    fn id(&self) -> &str {
        "filter"
    }

    fn parse(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpectedly no input, block needs input".to_string()))
    }
    
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, mut input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        let ExpressionKind::Iter(_, iter_transforms, _) = &mut input.kind else {
            return Err(span.error(ErrorKind::Syntax, "Expected iter as block input".to_string()))
        };

        let symbol = match parser.parse_expression(header)?.kind.clone() {
            ExpressionKind::Symbol(symbol) => symbol,
            _ => return Err(span.error(ErrorKind::Syntax, "Expected symbol for filter variable".to_string()))
        };
        let body = parser.iter_statement(body)
                                    .map_ok(Expression::return_if_forward)
                                    .try_collect()?;
        let id = TRANSFORM_FN_ID.load(Ordering::Relaxed);
        TRANSFORM_FN_ID.store(id+1, Ordering::Relaxed);
        let name = format!("__filter_function{}", id);
        let signature = Signature::new(None, &name, vec![symbol.ty], Type::Bool);

        let transform = IterTransform{
            kind: TransformKind::Filter,
            fun: Fn::new(signature, vec![symbol.name], body),
            span: span.clone()
        };
        iter_transforms.push(transform);

        Ok(input.kind)
    }
}