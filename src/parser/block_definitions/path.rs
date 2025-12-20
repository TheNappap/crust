use crate::{lexer::Span, parser::{Expression, ExpressionKind, Path, block_definitions::OperatorBlockDefintion, blocks::BlockTag, parse_ops::OperatorKind}, utils::{Result, ThrowablePosition}};


#[derive(Default)]
pub struct PathBlock;

impl OperatorBlockDefintion for PathBlock {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::ColonColon)
    }

    fn parse_binary_operator(&self, span: &Span, left: Expression, right: Expression) -> Result<ExpressionKind> {
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