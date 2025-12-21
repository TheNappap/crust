

use crate::{block_definitions::{OperatorBlockDefintion, TrailingGroup, array::Index, call::Call, data::Field}, lexer::{Delimeter, Span}, parser::{Expression, ExpressionKind, OperatorKind, PathElement}, utils::{Result, ThrowablePosition}};

#[derive(Default)]
pub struct Dot;

impl Dot {
    fn parse_dot_no_indexing(&self, span: &Span, right: Expression, left: Option<Expression>, params: Option<Vec<Expression>>) -> Result<ExpressionKind> {
        if let Some(left) = left {
            if let Some(params) = params {
                let mut path = right;
                let mut path_inner = match path.kind {
                    ExpressionKind::Path(path) => path,
                    ExpressionKind::Symbol(symbol) => symbol.name.into(),
                    _ => return span.syntax("Expected path expression".into()),
                };
                path_inner.add(PathElement::Inferred);
                path.kind = ExpressionKind::Path(path_inner);
                Call.parse_call(span, path, params, Some(left))
            } else {
                Field.parse_field(span, left, right)
            }
        } else {
            if let Some(params) = params {
                Call.parse_call(span, right, params, None)
            } else {
                Ok(right.kind)
            }
        }
    }

    fn parse_dot(&self, span: &Span, right: Expression, left: Option<Expression>, mut trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        let params = if let Some(TrailingGroup { delimeter: Delimeter::Parens, .. }) = trailing_groups.last() {
            let group = trailing_groups.pop().unwrap();
            Some(group.expressions())
        } else {
            None
        };

        let mut expression = self.parse_dot_no_indexing(span, right, left, params)?;

        while let Some(TrailingGroup{delimeter: Delimeter::Brackets, expressions}) = trailing_groups.pop() {
            let collection = Expression::new(expression, span.clone());
            expression = Index.parse_index(span, collection, expressions)?;
        }

        Ok(expression)
    }
}

impl OperatorBlockDefintion for Dot {
    fn operator(&self) -> OperatorKind {
        OperatorKind::Dot
    }

    fn allow_trailing_groups(&self) -> bool {
        true
    }

    fn parse_unary_operator(&self, span: &Span, right: Expression, trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        self.parse_dot(span, right, None, trailing_groups)
    }
    
    fn parse_binary_operator(&self, span: &Span, left: Expression, right: Expression, trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        self.parse_dot(span, right, Some(left), trailing_groups)
    }
}