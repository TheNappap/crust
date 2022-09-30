
use std::convert::identity;

use crate::{error::{Result, Error}, lexer::{Block, Token, Delimeter, Operator, BlockStream}, parser::block_definitions::{binary_ops::{Add, Multiply, Divide, Subtract, Equals, NotEquals}, BlockDefinition, unary_ops::Negate}};

enum OpKind {
    Prefix,
    //Postfix,
    Binary
}

fn precedence( kind: OpKind, op: &Operator) -> Option<u8> {
    use Operator::*;
    use OpKind::*;
    match (kind, op) {
        (Prefix, Dash | Not) => Some(1),
        (Binary, Star | Slash) => Some(2),
        (Binary, Plus | Dash) => Some(3),
        (Binary, EqEq | Neq) => Some(4),
        _ => None,
    }
}

fn id_of_operator(kind:OpKind, op: &Operator) -> Option<String> {
    use Operator::*;
    use OpKind::*;
    let id = match (kind, op) {
        (Prefix, Not) => todo!(),
        (Prefix, Dash) => Negate.id(),
        (Binary, Star) => Multiply.id(),
        (Binary, Slash) => Divide.id(),
        (Binary, Plus) => Add.id(),
        (Binary, Dash) => Subtract.id(),
        (Binary, EqEq) => Equals.id(),
        (Binary, Neq) => NotEquals.id(),
        _ => return None,
    }.to_string();
    Some(id)
}

fn next_operator_index(tokens: &Vec<Token>) -> Option<(usize, OpKind, Operator)> {
    let (_, op) = tokens.iter()
        .enumerate()
        .scan(None::<Token>, |last_token,(i,token)| {
            let out = match (&last_token, &token) {
                (None, Token::Operator(op)) => Some(Some((i,OpKind::Prefix,op))),
                (Some(Token::Operator(_)), Token::Operator(_)) => Some(None),
                (_,Token::Operator(op)) => Some(Some((i,OpKind::Binary,op))),
                (_,_) => Some(None),
            };
            *last_token = Some(token.clone());
            out
        })  
        .filter_map(identity)
        .fold((None, None), |acc, op| {
            match (acc.0, precedence(OpKind::Binary, op.2)) {
                (None, Some(pr)) => (Some(pr), Some(op)),
                (Some(acc), Some(pr)) if pr > acc => (Some(pr), Some(op)),
                _ => acc
            }
        });

    op.map(|(index,kind, op)| (index, kind, op.clone()))
}

pub fn parse_expression(tokens: Vec<Token>) -> Result<Block> {
    if let Some((index, kind, op)) = next_operator_index(&tokens) {
        if let Some(tag) = id_of_operator(kind, &op) {
            let mut tokens = tokens;
            if index == 0 {
                tokens.remove(0);
            } else {
                tokens[index] = Token::Operator(Operator::Comma);
            }
            return Ok(Block{tag, header: tokens, body: vec![], chain: None});
        }
    }

    let mut blocks = BlockStream::new(tokens);
    let block = blocks.next();
    match block {
        None => return Err(Error::syntax("Expected an expression".to_string(), 0)),
        Some(Err(err)) => Err(err),
        Some(Ok(block)) => if blocks.next().is_some() {
            return Err(Error::syntax("Unexpected block after expression".to_string(), 0));
        } else {
            Ok(block)
        }
    }
}

pub struct TokenList {
    pub contents: Vec<Vec<Token>>
}

impl TokenList {
    fn from(tokens: Vec<Token>) -> TokenList {
        let mut tokens = tokens.into_iter();
        let mut contents = Vec::new();
        while let Some(element) = Self::take_element(&mut tokens) {
            contents.push(element)
        }
        TokenList { contents }
    }

    fn take_element(tokens: &mut impl Iterator<Item=Token>) -> Option<Vec<Token>> {
        let mut element = Vec::new();
        loop {
            match tokens.next() {
                None | Some(Token::Operator(Operator::Comma)) => break,
                Some(token) => element.push(token)
            }
        }
        if element.is_empty() { None } else { Some(element) }
    }
}

pub fn parse_list(tokens: Vec<Token>) -> TokenList {
    let tokens = if tokens.len() == 1 {
        if let Some(Token::Group(Delimeter::Parens, tokens)) = tokens.first() {
            tokens.clone()
        } else { tokens }
    } else { tokens };

    TokenList::from(tokens)
}