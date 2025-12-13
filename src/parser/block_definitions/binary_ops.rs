use itertools::Itertools;

use crate::{error::{ErrorKind, Result, ThrowablePosition}, lexer::{Span, Token}, parser::{BinOpKind, Expression, ExpressionKind, Parser, Type}};

use super::BlockDefinition;


#[derive(Default)]
pub struct Add;

impl BlockDefinition for Add {
    fn id(&self) -> &str {
        "add"
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
pub struct Subtract;

impl BlockDefinition for Subtract {
    fn id(&self) -> &str {
        "sub"
    }

    fn parse(&self, span: &Span, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        if operands.len() != 2 {
            return Err(span.error(ErrorKind::Syntax, "Subtract operator expects exactly 2 operands".to_string()));
        }

        Ok(ExpressionKind::BinOp(BinOpKind::Sub, Box::new(operands.remove(0)), Box::new(operands.remove(0)), Type::Inferred))
    }
    
    fn parse_chained(&self, span: &Span, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<ExpressionKind> {
        Err(span.error(ErrorKind::Syntax, "Unexpected input, block doesn't handle input".to_string()))
    }
}


#[derive(Default)]
pub struct Multiply;

impl BlockDefinition for Multiply {
    fn id(&self) -> &str {
        "mul"
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
    fn id(&self) -> &str {
        "div"
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
    fn id(&self) -> &str {
        "eq"
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
    fn id(&self) -> &str {
        "neq"
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