use crate::{
    error::{Error, Result},
    lexer::{Block, Delimeter, Token},
    parser::{Expression, Fn, Parser, Type, syntax_tree::fn_expr::Signature},
};

use super::BlockDefinition;

pub struct FnDef;

impl BlockDefinition for FnDef {
    fn id(&self) -> &str {
        "fn"
    }

    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression> {
        assert!(block.tag == self.id());
        let mut tokens = block.header.into_iter();

        let name = match tokens.next() {
            Some(Token::Ident(value)) => value,
            _ => {
                return Err(
                    Error::syntax("Expected an identifier as function name".to_string(), 0).into(),
                )
            }
        };

        let _params = match tokens.next() {
            Some(Token::Group(Delimeter::Parens, params)) => params,
            _ => return Err(Error::syntax("Expected parameters in parens".to_string(), 0).into()),
        };

        let returns = match (tokens.next(), tokens.next(), tokens.next()) {
            (None, None, None) => Type::Void,
            (Some(Token::Symbol('-')), Some(Token::Symbol('>')), Some(Token::Ident(ty))) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                _ => return Err(Error::syntax("Unknown return type".to_string(), 0).into())
            },
            _ => return Err(Error::syntax("Unexpected symbols after function header".to_string(), 0).into()),
        };

        let signature = Signature::new(&name, vec![], returns);
        let exprs = block
            .body
            .into_iter()
            .map(|block| parser.parse_block(block))
            .collect::<Result<Vec<Expression>>>()?;
        Ok(Expression::Fn(Fn::new(signature, exprs)))
    }
}
