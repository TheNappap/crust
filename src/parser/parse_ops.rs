
use std::convert::identity;

use crate::{lexer::{Token, Delimeter, Operator}, parser::block_definitions::{binary_ops::{Add, Multiply, Divide, Subtract, Equals, NotEquals}, BlockDefinition, unary_ops::Negate, dot}};

#[derive(Debug, PartialEq, Clone)]
enum OpKind {
    Prefix,
    //Postfix,
    Binary
}

fn precedence( kind: OpKind, op: &Operator) -> Option<u8> {
    use Operator::*;
    use OpKind::*;
    match (kind, op) {
        (Binary, Dot) => Some(0),
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
        (Prefix, Not) => Negate.id(),
        (Prefix, Dash) => Negate.id(),
        (Binary, Dot) => dot::Dot.id(),
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
            match (acc.0, precedence(op.1.clone(), op.2)) {
                (None, Some(pr)) => (Some(pr), Some(op)),
                (Some(acc), Some(pr)) if pr > acc => (Some(pr), Some(op)),
                _ => acc
            }
        });

    op.map(|(index,kind, op)| (index, kind, op.clone()))
}

fn parse_indexing(tokens: &mut Vec<Token>) {
    if let Token::Group(Delimeter::Brackets, _) = tokens.last().unwrap() {
        tokens.insert(0, Token::Ident("index".into()));
    }
}

pub fn parse_operators(tokens: &mut Vec<Token>)  {
    if tokens.is_empty() {
        return;
    }

    if let Some((index, kind, op)) = next_operator_index(&tokens) {
        if let Some(tag) = id_of_operator(kind, &op) {
            if index == 0 {
                tokens.remove(0);
            } else {
                tokens[index] = Token::Operator(Operator::Comma);
            }
            tokens.insert(0, Token::Ident(tag));
            return;
        }
    }

    parse_indexing(tokens);
}