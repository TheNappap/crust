
use core::fmt;
use std::convert::identity;

use crate::lexer::{Delimeter, Token, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum OperatorKind {
    Dot,
    Not,
    Star, Slash,
    Plus, Dash,
    EqEq, Neq, 
    Less, LessEq,
    Great, GreatEq,
    Range,   
}

impl OperatorKind {
    pub fn from(token: &TokenKind) -> Option<Self> {
        match token {
            TokenKind::Dot => Some(OperatorKind::Dot),
            TokenKind::Plus => Some(OperatorKind::Plus),
            TokenKind::Dash => Some(OperatorKind::Dash),
            TokenKind::Star => Some(OperatorKind::Star),
            TokenKind::Slash => Some(OperatorKind::Slash),
            TokenKind::EqEq => Some(OperatorKind::EqEq),
            TokenKind::Neq => Some(OperatorKind::Neq),
            TokenKind::Not => Some(OperatorKind::Not),
            TokenKind::Less => Some(OperatorKind::Less),
            TokenKind::LessEq => Some(OperatorKind::LessEq),
            TokenKind::Great => Some(OperatorKind::Great),
            TokenKind::GreatEq => Some(OperatorKind::GreatEq),
            TokenKind::Range => Some(OperatorKind::Range),
            _ => None,
        }
    }
}

impl fmt::Display for OperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use OperatorKind::*;
        match self {
            Not => write!(f, "!"),
            Dash => write!(f, "-"),
            Dot => write!(f, "."),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Plus => write!(f, "+"),
            EqEq => write!(f, "=="),
            Neq => write!(f, "!="),
            Less => write!(f, "<"),
            LessEq => write!(f, "<="),
            Great => write!(f, ">"),
            GreatEq => write!(f, ">="),
            Range => write!(f, ".."),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum OperatorPosition {
    Prefix,
    _Postfix,
    Binary
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Operator {
    kind: OperatorKind,
    position: OperatorPosition,
}

impl Operator {
    pub fn from(token: &TokenKind) -> Option<Self> {
        if let Some(kind) = OperatorKind::from(&token) {
            Some(Self { kind, position: OperatorPosition::Binary })
        } else {
            None
        }
    }

    fn precedence(&self) -> Option<u8> {
        use OperatorKind::*;
        use OperatorPosition::*;
        match (&self.position, &self.kind) {
            (Binary, Dot) => Some(0),
            (Prefix, Dash | Not) => Some(1),
            (Binary, Star | Slash) => Some(2),
            (Binary, Plus | Dash) => Some(3),
            (Binary, EqEq | Neq | Less | LessEq | Great | GreatEq) => Some(4),
            (Binary, Range) => Some(5),
            _ => None,
        }
    }
}

struct IndexedOperator {
    index: usize,
    token: Token,
    operator: Operator,
}

impl IndexedOperator {
    fn new(index: usize, token: Token) -> Option<Self> {
        if let Some(operator) = Operator::from(&token.kind) {
            Some(Self { index, token, operator })
        } else {
            None
        }
    }

    fn as_unary(mut self) -> Self {
        self.operator.position = OperatorPosition::Prefix;
        self
    }

    fn as_binary(mut self) -> Self {
        self.operator.position = OperatorPosition::Binary;
        self
    }

    fn as_token(self) -> Token {
        self.token
    }
}

fn next_operator_index(tokens: &Vec<Token>) -> Option<IndexedOperator> {
    let op = tokens.iter()
        .enumerate()
        .scan(None::<TokenKind>, |last_token,(index,token)| {
            let previous_token = last_token.clone();
            let current_token = token.kind.clone();
            *last_token = Some(current_token.clone());
            let Some(operator) = IndexedOperator::new(index, token.clone()) else {
                return Some(None);
            };
            match (previous_token, operator) {
                (Some(token), _) if token.is_operator() => Some(None),
                (None, op) => Some(Some(op.as_unary())),
                (_, op) => Some(Some(op.as_binary())),
            }
        })  
        .filter_map(identity)
        .fold(None, |acc: Option<(u8, IndexedOperator)>, op| {
            let precedence = match (acc, op.operator.precedence()) {
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

impl TokenKind {
    fn is_operator(&self) -> bool {
        OperatorKind::from(self).is_some()
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
        let mut token = operator.as_token();
        token.span = span;
        tokens.insert(0, token);
        return;
    }

    parse_indexing(tokens);
}