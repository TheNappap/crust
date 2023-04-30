use crate::lexer::{Token, Operator, Delimeter, TokenKind};



fn split_list(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let mut tokens = tokens.into_iter().filter(|t| !matches!(t.kind, TokenKind::NewLine));
    let mut contents = Vec::new();
    while let Some(element) = take_element(&mut tokens) {
        contents.push(element)
    }
    contents
}

fn take_element(tokens: &mut impl Iterator<Item=Token>) -> Option<Vec<Token>> {
    let mut element = Vec::new();
    loop {
        match tokens.next() {
            None | Some(Token{kind: TokenKind::Operator(Operator::Comma), ..}) => break,
            Some(token) => element.push(token)
        }
    }
    if element.is_empty() { None } else { Some(element) }
}

pub fn parse_list(tokens: Vec<Token>) -> Vec<Vec<Token>> {
    let tokens = if tokens.len() == 1 {
        if let Some(Token{kind: TokenKind::Group(Delimeter::Parens, tokens), ..}) = tokens.first() {
            tokens.clone()
        } else { tokens }
    } else { tokens };

    split_list(tokens)
}