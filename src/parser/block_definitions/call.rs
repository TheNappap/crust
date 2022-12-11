use itertools::Itertools;

use crate::{
    error::{Error, Result},
    lexer::{Delimeter, Token},
    parser::{
        syntax_tree::{Expression},
        Parser, Type, Signature
    },
};

use super::BlockDefinition;

#[derive(Default)]
pub struct Call;

impl BlockDefinition for Call {
    fn id(&self) -> &str {
        "call"
    }

    fn parse(&self, header: Vec<Token>, _body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        use crate::lexer::Operator::*;
        use Token::*;
        use Delimeter::*;
        let (ty, name, tokens) = match header.as_slice() {
            [Ident(name), Group(Parens, tokens)] => (None, name.to_owned(), tokens),
            [Ident(type_name), Operator(ColonColon), Ident(name), Group(Parens, tokens)] => 
                (Some(Type::Named(type_name.to_owned())), name.to_owned(), tokens),
            _ => return Err(Error::syntax("Badly formed call expression".to_string(), 0)),
        };

        let exprs = parser.parse_list(tokens.clone())
            .into_iter()
            .map(|tokens| parser.parse_expression(tokens))
            .try_collect()?;
        Ok(Expression::Call(Signature::new(ty, &name, vec![], Type::Inferred), exprs))
    }
    
    fn parse_chained(&self, _: Vec<Token>, _: Vec<Token>, _: Expression, _: &Parser) -> Result<Expression> {
        Err(Error::syntax("Unexpected input, block doesn't handle input".to_string(), 0))
    }
}
