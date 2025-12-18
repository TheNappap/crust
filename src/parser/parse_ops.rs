
use std::convert::identity;

use crate::{lexer::{Delimeter, Token, TokenKind}, parser::block_definitions::{BlockDefinition, binary_ops::{Add, Divide, Equals, GreatEquals, GreatThan, LessEquals, LessThan, Multiply, NotEquals, Subtract}, dot, range, unary_ops::Negate}};

#[derive(Debug, PartialEq, Clone)]
enum OpKind {
    Prefix,
    //Postfix,
    Binary
}

fn is_operator(token: &TokenKind) -> bool {
    use TokenKind::*;
    match token {
        Dot | Not => true,
        Star | Slash => true,
        Plus | Dash => true,
        EqEq | Neq | Less | LessEq | Great | GreatEq => true,
        Range => true,
        _ => false,
    }
}

fn precedence( kind: &OpKind, op: &TokenKind) -> Option<u8> {
    use TokenKind::*;
    use OpKind::*;
    match (kind, op) {
        (Binary, Dot) => Some(0),
        (Prefix, Dash | Not) => Some(1),
        (Binary, Star | Slash) => Some(2),
        (Binary, Plus | Dash) => Some(3),
        (Binary, EqEq | Neq | Less | LessEq | Great | GreatEq) => Some(4),
        (Binary, Range) => Some(5),
        _ => None,
    }
}

fn id_of_operator(kind: &OpKind, op: &TokenKind) -> Option<String> {
    use TokenKind::*;
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
        (Binary, Less) => LessThan.id(),
        (Binary, LessEq) => LessEquals.id(),
        (Binary, Great) => GreatThan.id(),
        (Binary, GreatEq) => GreatEquals.id(),
        (Binary, Range) => range::Range.id(),
        _ => return None,
    }.to_string();
    Some(id)
}

fn next_operator_index(tokens: &Vec<Token>) -> Option<(usize, String)> {
    let op = tokens.iter()
        .enumerate()
        .scan(None::<Token>, |last_token,(i,token)| {
            let out = match (&last_token, &token.kind) {
                (_, token) if !is_operator(token) => Some(None),
                (Some(Token{kind: token, ..}), _) if is_operator(token) => Some(None),
                (None, op) => Some(Some((i,OpKind::Prefix,op))),
                (_, op) => Some(Some((i,OpKind::Binary,op))),
            };
            *last_token = Some(token.clone());
            out
        })  
        .filter_map(identity)
        .fold(None, |acc: Option<(u8, usize, String)>, (i, kind, op)| {
            let precedence = precedence(&kind, &op);
            let precedence = match (acc, precedence) {
                (None, Some(pr)) => pr,
                (Some((acc, _, _)), Some(pr)) if pr > acc => pr,
                (acc, _) => return acc,
            };
            Some((precedence, i, id_of_operator(&kind, op)?))
        });

    op.and_then(|(_, index, id)| Some((index, id)) )
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
    if !is_operator(&tokens[0].kind) && !is_operator(&tokens[1].kind) && !matches!(tokens[1].kind, TokenKind::Group(Delimeter::Brackets, _)) {
        return;
    }

    if let Some((index, tag)) = next_operator_index(&tokens) {
        let span = tokens.iter().map(|t|t.span.clone()).fold(tokens.first().unwrap().span.clone(), |acc, s| acc.union(&s));
        if index == 0 {
            tokens.remove(0);
        } else {
            tokens[index] = Token::new(TokenKind::Comma, tokens[index].span.clone());
        }
        tokens.insert(0, Token::new(TokenKind::Ident(tag), span));
        return;
    }

    parse_indexing(tokens);
}