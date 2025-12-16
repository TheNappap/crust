use itertools::PeekingNext;

use crate::{lexer::{Delimeter, Operator, Token, TokenKind, TokenStream}, parser::blocks::{Block, FromVecStream, block_collector::BlockCollector}, utils::{Peeking, Result}};

use super::block_tokens::BlockTokenStream;


pub struct BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    stream: BlockTokenStream<Stream>,
    peeked: Option<Result<Block>>,
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
        }
    }
}

impl From<Vec<Token>> for BlockStream<FromVecStream> {
    fn from(tokens: Vec<Token>) -> Self {
        BlockStream {
            stream: BlockTokenStream::from(tokens),
            peeked: None,
        }
    }
}

impl<Stream> BlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    pub fn forward_last(self) -> ForwardBlockStream<Stream> {
        self.into()
    }

    fn take_block(&mut self) -> Result<Option<Block>> {
        let Some(tokens) = self.stream.next() else {
            return Ok(None);
        };
        BlockCollector::from(tokens?).collect_block()
    }
}

pub struct ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    stream: BlockTokenStream<Stream>,
    peeked: Option<Result<(Block, bool)>>,
}

impl<Stream> Iterator for ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    type Item = Result<(Block, bool)>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            next @ Some(_) => next,
            None => self.take_block().transpose(),
        }
    }
}

impl<Stream> PeekingNext for ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
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

impl<Stream> Peeking for ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn peek(&mut self) -> Option<&Self::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

impl From<Vec<Token>> for ForwardBlockStream<FromVecStream> {
    fn from(tokens: Vec<Token>) -> Self {
        ForwardBlockStream {
            stream: BlockTokenStream::from(tokens),
            peeked: None,
        }
    }
}

impl<Stream> From<BlockStream<Stream>> for ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn from(stream: BlockStream<Stream>) -> Self {
        ForwardBlockStream {
            stream: stream.stream,
            peeked: None,
        }
    }
}

impl<Stream> ForwardBlockStream<Stream> where Stream: Peeking<Item=Result<Token>> {
    fn take_block(&mut self) -> Result<Option<(Block, bool)>> {
        let Some(tokens) = self.stream.next().transpose()? else {
            return Ok(None);
        };
        let is_last_block = self.stream.peek().is_none();
        if is_last_block {
            let last_token = tokens.last().expect("`tokens` is already checked to be none empty");
            if !matches!(last_token.kind, TokenKind::Operator(Operator::Semicolon) | TokenKind::Group(Delimeter::Braces, _)) {
                return Ok(Some( (Block::anonymous_block(tokens), true) ));
            }
        }

        let block = BlockCollector::from(tokens).collect_block()?;
        return Ok(block.map(|b| (b, false)))
    }
}