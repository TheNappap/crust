
use std::convert::identity;

use crate::{lexer::{Delimeter, Operator, Token, TokenKind}, parser::block_definitions::{binary_ops::{Add, Divide, Equals, Multiply, NotEquals, Subtract}, dot, range, unary_ops::Negate, BlockDefinition}};

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
        (Binary, Range) => Some(5),
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
        (Binary, Range) => range::Range.id(),
        _ => return None,
    }.to_string();
    Some(id)
}

fn next_operator_index(tokens: &Vec<Token>) -> Option<(usize, OpKind, Operator)> {
    let (_, op) = tokens.iter()
        .enumerate()
        .scan(None::<Token>, |last_token,(i,token)| {
            let out = match (&last_token, &token.kind) {
                (None, TokenKind::Operator(op)) => Some(Some((i,OpKind::Prefix,op))),
                (Some(Token{kind: TokenKind::Operator(_), ..}), TokenKind::Operator(_)) => Some(None),
                (_,TokenKind::Operator(op)) => Some(Some((i,OpKind::Binary,op))),
                (_,_) => Some(None),
            };
            *last_token = Some(token.clone());
            out
        })  
        .filter_map(identity)
        .fold((None, None), |acc, op| {
            match (acc.0, precedence(op.1.clone(), &op.2)) {
                (None, Some(pr)) => (Some(pr), Some(op)),
                (Some(acc), Some(pr)) if pr > acc => (Some(pr), Some(op)),
                _ => acc
            }
        });

    op.map(|(index,kind, op)| (index, kind, op.clone()))
}

fn parse_indexing(tokens: &mut Vec<Token>) {
    let last_token = tokens.last().unwrap();
    if let TokenKind::Group(Delimeter::Brackets, _) = last_token.kind {
        let new_token = Token::new(TokenKind::Ident("index".into()), last_token.span.clone());
        tokens.insert(0, new_token);
    }
}

pub fn parse_operators(tokens: &mut Vec<Token>)  {
    if tokens.is_empty() {
        return;
    }
    if !matches!(tokens[0].kind, TokenKind::Operator(_)) && !matches!(tokens[1].kind, TokenKind::Operator(_) | TokenKind::Group(Delimeter::Brackets, _)) {
        return;
    }

    if let Some((index, kind, op)) = next_operator_index(&tokens) {
        let span = tokens.iter().map(|t|t.span.clone()).fold(tokens.first().unwrap().span.clone(), |acc, s| acc.union(&s));
        if let Some(tag) = id_of_operator(kind, &op) {
            if index == 0 {
                tokens.remove(0);
            } else {
                let new_token = Token::new(TokenKind::Operator(Operator::Comma), tokens[index].span.clone());
                tokens[index] = new_token;
            }
            tokens.insert(0, Token::new(TokenKind::Ident(tag), span));
            return;
        }
    }

    parse_indexing(tokens);
}