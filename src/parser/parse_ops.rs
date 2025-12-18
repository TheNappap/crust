
use std::convert::identity;

use crate::{lexer::{Delimeter, Token, TokenKind}, parser::block_definitions::{BlockDefinition, binary_ops::{Add, Divide, Equals, GreatEquals, GreatThan, LessEquals, LessThan, Multiply, NotEquals, Subtract}, dot, range, unary_ops::Negate}};

impl TokenKind {
    fn is_operator(&self) -> bool {
        use TokenKind::*;
        match self {
            Dot | Not => true,
            Star | Slash => true,
            Plus | Dash => true,
            EqEq | Neq | Less | LessEq | Great | GreatEq => true,
            Range => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum OpKind {
    Prefix,
    //Postfix,
    Binary
}

struct Operator {
    index: usize,
    token: TokenKind,
    kind: OpKind,
}

impl Operator {
    fn new(index: usize, token: TokenKind) -> Option<Self> {
        if token.is_operator() {
            Some(Operator { index, token, kind: OpKind::Binary })
        } else {
            None
        }
    }

    fn as_unary(mut self) -> Self {
        self.kind = OpKind::Prefix;
        self
    }

    fn as_binary(mut self) -> Self {
        self.kind = OpKind::Binary;
        self
    }

    fn precedence(&self) -> Option<u8> {
        use TokenKind::*;
        use OpKind::*;
        match (&self.kind, &self.token) {
            (Binary, Dot) => Some(0),
            (Prefix, Dash | Not) => Some(1),
            (Binary, Star | Slash) => Some(2),
            (Binary, Plus | Dash) => Some(3),
            (Binary, EqEq | Neq | Less | LessEq | Great | GreatEq) => Some(4),
            (Binary, Range) => Some(5),
            _ => None,
        }
    }

    fn id_of_operator(&self) -> String {
        use TokenKind::*;
        use OpKind::*;
        let id = match (&self.kind, &self.token) {
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
            _ => unreachable!(),
        }.to_string();
        id
    }
}

fn next_operator_index(tokens: &Vec<Token>) -> Option<Operator> {
    let op = tokens.iter()
        .enumerate()
        .scan(None::<TokenKind>, |last_token,(index,token)| {
            let previous_token = last_token.clone();
            let current_token = token.kind.clone();
            *last_token = Some(current_token.clone());
            let Some(operator) = Operator::new(index, current_token) else {
                return Some(None);
            };
            match (previous_token, operator) {
                (Some(token), _) if token.is_operator() => Some(None),
                (None, op) => Some(Some(op.as_unary())),
                (_, op) => Some(Some(op.as_binary())),
            }
        })  
        .filter_map(identity)
        .fold(None, |acc: Option<(u8, Operator)>, op| {
            let precedence = match (acc, op.precedence()) {
                (None, Some(pr)) => pr,
                (Some((acc, _)), Some(pr)) if pr > acc => pr,
                (acc, _) => return acc,
            };
            Some((precedence, op))
        });

    op.map(|(_, op)| op )
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
    if !&tokens[0].kind.is_operator() && !&tokens[1].kind.is_operator() && !matches!(tokens[1].kind, TokenKind::Group(Delimeter::Brackets, _)) {
        return;
    }

    if let Some(operator) = next_operator_index(&tokens) {
        let span = tokens.iter().map(|t|t.span.clone()).fold(tokens.first().unwrap().span.clone(), |acc, s| acc.union(&s));
        if operator.index == 0 {
            tokens.remove(0);
        } else {
            tokens[operator.index] = Token::new(TokenKind::Comma, tokens[operator.index].span.clone());
        }
        tokens.insert(0, Token::new(TokenKind::Ident(operator.id_of_operator()), span));
        return;
    }

    parse_indexing(tokens);
}