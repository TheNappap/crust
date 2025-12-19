use itertools::PeekingNext;

use crate::{lexer::{Token, TokenKind, TokenStream}, parser::blocks::{Block, BlockTag, FromVecStream, block_collector::BlockCollector}, utils::{Peeking, Result}};

use super::block_tokens::BlockTokenStream;


pub struct BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    stream: BlockTokenStream<Stream>,
    peeked: Option<Result<Block>>,
    forward_tag: Option<BlockTag>,
    collect_operators: bool,
}

impl<Stream> Iterator for BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    type Item = Result<Block>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            next @ Some(_) => next,
            None => self.take_block().transpose(),
        }
    }
}

impl<Stream> PeekingNext for BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
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

impl<Stream> Peeking for BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn peek(&mut self) -> Option<&Self::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

impl<'str> From<&'str str> for BlockStream<TokenStream<'str>> {
    fn from(source: &'str str) -> Self {
        BlockStream {
            stream: BlockTokenStream::from(source),
            peeked: None,
            forward_tag: None,
            collect_operators: false,
        }
    }
}

impl From<Vec<Token>> for BlockStream<FromVecStream> {
    fn from(tokens: Vec<Token>) -> Self {
        BlockStream {
            stream: BlockTokenStream::from(tokens),
            peeked: None,
            forward_tag: None,
            collect_operators: false,
        }
    }
}

impl<Stream> BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    pub fn collect_operators(mut self, collect_operators: bool) -> BlockStream<Stream> {
        self.collect_operators = collect_operators;
        self
    }

    pub fn forward_last(mut self, tag: BlockTag) -> BlockStream<Stream> {
        self.forward_tag = Some(tag);
        self
    }

    fn take_block(&mut self) -> Result<Option<Block>> {
        let Some(tokens) = self.stream.next().transpose()? else {
            return Ok(None);
        };
        let is_last_block = self.stream.peek().is_none();
        if is_last_block && let Some(forward_tag) = &self.forward_tag {
            let last_token = tokens.last().expect("`tokens` is already checked to be none empty");
            if !matches!(last_token.kind, TokenKind::Semicolon) {
                let mut forward_block = Block::anonymous_block(tokens);
                forward_block.tag = forward_tag.clone();
                return Ok(Some(forward_block));
            }
        }

        BlockCollector::from(tokens).collect_operators(self.collect_operators).collect_block()
    }
}