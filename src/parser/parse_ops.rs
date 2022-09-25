
use crate::{error::{Result, Error}, lexer::{Block, Token, Delimeter, Operator, BlockStream}, parser::block_definitions::{binary_ops::{Add, Multiply, Divide, Subtract, Equals, NotEquals}, BlockDefinition}};

fn precedence(op: &Operator) -> Option<u8> {
    use Operator::*;
    match op {
        //Dash | Excl => Some(1), TODO unary ops
        Star | Slash => Some(2),
        Plus | Dash => Some(3),
        Eq | Neq => Some(4),
        _ => None,
    }
}

fn id_of_operator(op: &Operator) -> Option<String> {
    use Operator::*;
    Some(match op {
        Excl => todo!(),
        Star => Multiply.id(),
        Slash => Divide.id(),
        Plus => Add.id(),
        Dash => Subtract.id(),
        Eq => Equals.id(),
        Neq => NotEquals.id(),
        _ => todo!(),
    }.to_string())
}

pub fn parse_expression(tokens: Vec<Token>) -> Result<Block> {
    let (op_index, _, op) = tokens.iter()
        .enumerate()
        .filter_map(|(i,token)| if let Token::Operator(op) = token { Some((i,op)) } else {None})    
        .fold((0, None, None), |acc, op| {
            match (acc.1, precedence(op.1), op.0) {
                (None, Some(pr), i) => (i, Some(pr), Some(op.1)),
                (Some(acc), Some(pr), i) if pr > acc => (i, Some(pr), Some(op.1)),
                _ => acc
            }
        });

    if let Some(op) = op {
        if let Some(tag) = id_of_operator(op) {
            let mut tokens = tokens;
            if op_index == 0 {
                tokens.remove(0);
            } else {
                tokens[op_index] = Token::Operator(Operator::Comma);
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