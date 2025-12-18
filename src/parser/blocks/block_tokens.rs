
use itertools::{Itertools, PeekingNext};

use crate::lexer::{Delimeter, Token, TokenKind, TokenStream};
use crate::parser::blocks::{FromVecStream, from_vec_stream};
use crate::utils::{Peeking, Result};

pub struct BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    stream: Stream,
    peeked: Option<Result<Vec<Token>>>,
}

impl<'str> From<&'str str> for BlockTokenStream<TokenStream<'str>> {
    fn from(source: &'str str) -> Self {
        BlockTokenStream {
            stream: TokenStream::from(source),
            peeked: None,
        }
    }
}

impl From<Vec<Token>> for BlockTokenStream<FromVecStream> {
    fn from(tokens: Vec<Token>) -> Self {
        BlockTokenStream {
            stream: from_vec_stream(tokens),
            peeked: None,
        }
    }
}

impl<Stream> From<Stream> for BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn from(tokens: Stream) -> Self {
        BlockTokenStream {
            stream: tokens,
            peeked: None,
        }
    }
}

impl<Stream> BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    pub fn collect_block_tokens(&mut self) -> Result<Option<Vec<Token>>> {
        self.trim_new_lines();
        let mut ends_on_braces = false;
        let mut tokens: Vec<_> = (&mut self.stream)
            .take_while_inclusive(|token| match token {
                Ok(Token{kind:TokenKind::Semicolon, ..}) => false,
                Ok(Token{kind:TokenKind::Group(Delimeter::Braces, _), ..}) => {
                    ends_on_braces = true;
                    false
                },
                Err(_) => false,
                _ => true
            })
            .try_collect()?;

        if tokens.is_empty() {
            return Ok(None);
        }

        // check for chain tokens
        match self.stream.peek() {
            _ if !ends_on_braces => {}
            Some(Ok(Token{kind:TokenKind::NewLine, ..})) => {}
            _ => {
                if let Some(chain_tokens) = self.collect_block_tokens()? {
                    tokens.extend(chain_tokens);
                }
            }
        }
        self.trim_new_lines();
        assert!(!tokens.is_empty());
        Ok(Some(tokens))
    }

    fn trim_new_lines(&mut self) {
        self.stream.peeking_take_while(|token| {
                matches!(token, Ok(Token { kind: TokenKind::NewLine, .. }))
            })
            .for_each(drop);
    }
}

impl<Stream> Iterator for BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    type Item = Result<Vec<Token>>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            next @ Some(_) => next,
            None => self.collect_block_tokens().transpose(),
        }
    }
}

impl<Stream> PeekingNext for BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
    where
        F: FnOnce(&Self::Item) -> bool,
    {
        if let Some(r) = self.peek() {
            if !accept(r) {
                return None;
            }
        }
        self.next()
    }
}

impl<Stream> Peeking for BlockTokenStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn peek(&mut self) -> Option<&Self::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}