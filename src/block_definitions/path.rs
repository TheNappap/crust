use crate::{block_definitions::{OperatorBlockDefintion, TrailingGroup}, lexer::Span, parser::{Expression, ExpressionKind, OperatorKind, Path}, utils::{Result, ThrowablePosition}};


#[derive(Default)]
pub struct PathBlock;

impl OperatorBlockDefintion for PathBlock {
    fn operator(&self) -> OperatorKind {
        OperatorKind::ColonColon
    }

    fn parse_binary_operator(&self, span: &Span, left: Expression, right: Expression, trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        assert!(trailing_groups.is_empty());
        let path = match (left.kind, right.kind) {
            (ExpressionKind::Symbol(symbol), ExpressionKind::Symbol(end_symbol)) => {
                let mut new_path = Path::from(end_symbol.name);
                new_path.add(symbol.name.into());
                new_path
            }
            (ExpressionKind::Symbol(symbol), ExpressionKind::Path(path)) => {
                let mut new_path = path;
                new_path.add(symbol.name.into());
                new_path
            }
            (ExpressionKind::Path(path), ExpressionKind::Symbol(symbol)) => {
                let mut new_path = Path::from(symbol.name);
                new_path.extend(path);
                new_path
            }
            _ => return span.syntax("Could not form path expression".into()),
        };
        Ok(ExpressionKind::Path(path))
    }
}