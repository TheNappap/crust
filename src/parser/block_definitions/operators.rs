use itertools::Itertools;

use crate::{lexer::{Span, Token}, parser::{BinOpKind, Expression, ExpressionKind, Parser, Type, UnOpKind, blocks::BlockTag, parse_ops::OperatorKind}, utils::{ErrorKind, Result, ThrowablePosition}};

use super::BlockDefinition;

#[derive(Default)]
pub struct Not;

impl BlockDefinition for Not {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Not)
    }

    fn parse(&self, _span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let operand = parser.parse_expression(header);
        Ok(ExpressionKind::UnOp(UnOpKind::Neg, Box::new(operand?), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Plus)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Add operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Add, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Dash;

impl BlockDefinition for Dash {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Dash)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;

        match operands.len() {
            1 => Ok(ExpressionKind::UnOp(UnOpKind::Neg, Box::new(operands.remove(0)), Type::Inferred)),
            2 => Ok(ExpressionKind::BinOp(BinOpKind::Sub, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred)),
            _ => Err(span.error(ErrorKind::Syntax, "Dash operator expects 1 or 2 operands".to_string()))
        }
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Multiply;

impl BlockDefinition for Multiply {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Star)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Multiply operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Mul, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Divide;

impl BlockDefinition for Divide {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Slash)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Divide operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Div, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct Equals;

impl BlockDefinition for Equals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::EqEq)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Equals operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Eq, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct NotEquals;

impl BlockDefinition for NotEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Neq)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "NotEquals operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Neq, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct LessThan;

impl BlockDefinition for LessThan {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Less)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "LessThan operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Less, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct LessEquals;

impl BlockDefinition for LessEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::LessEq)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "LessEquals operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::LessEq, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct GreatThan;

impl BlockDefinition for GreatThan {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Great)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "GreatThan operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Great, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}

#[derive(Default)]
pub struct GreatEquals;

impl BlockDefinition for GreatEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::GreatEq)
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "GreatEquals operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::GreatEq, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}