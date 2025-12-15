use std::iter::Peekable;

use itertools::PeekingNext;

pub trait Peeking : PeekingNext {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: Iterator> Peeking for Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        self.peek()
    }
}