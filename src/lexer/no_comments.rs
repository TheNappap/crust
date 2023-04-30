use itertools::{Itertools, PeekingNext};
use std::str::Chars;

use crate::error::{Result, ThrowablePosition};

use super::span::Position;

pub struct NoCommentsStream<'str> {
    stream: PositionStream<'str>,
    peeked: Option<Result<char>>,
}

impl<'str> NoCommentsStream<'str> {
    pub fn from(source: &'str str) -> NoCommentsStream<'str> {
        NoCommentsStream {
            stream: PositionStream::from(source),
            peeked: None,
        }
    }

    pub fn cur_pos(&self) -> Position {
        self.stream.cur_pos()
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

impl<'str> Iterator for NoCommentsStream<'str> {
    type Item = Result<char>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.peeked.take() {
            next @ Some(_) => next,
            None => Ok(self.stream.next()).transpose(),
        };

        match next {
            Some(Ok('/')) => self.take_on_slash(),
            c => c,
        }
    }
}

impl<'str> PeekingNext for NoCommentsStream<'str> {
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

impl<'str> NoCommentsStream<'str> {
    fn take_on_slash(&mut self) -> Option<Result<char>> {
        match self.stream.peek() {
            Some('/') => {
                self.take_line_comment();
                self.next()
            }
            Some('*') => self
                .take_block_comment()
                .and_then(|_| self.next().transpose())
                .transpose(),
            None => None,
            _ => Some(Ok('/')),
        }
    }

    fn take_line_comment(&mut self) {
        self.stream.peeking_take_while(|c| *c != '\n').count();
    }

    fn take_block_comment(&mut self) -> Result<()> {
        loop {
            match (self.stream.next(), self.stream.peek()) {
                (Some('*'), Some('/')) => {
                    self.stream.next();
                    return Ok(());
                }
                (Some('/'), _) => {
                    self.take_on_slash().transpose()?;
                }
                (None, _) => return self.cur_pos().lexer(r#"Expected '*/'"#.into()),
                _ => (),
            }
        }
    }
}

struct PositionStream<'str> {
    chars: Chars<'str>,
    peeked: Option<char>,
    cur_pos: Position,
}

impl<'str> PositionStream<'str> {
    fn from(source: &'str str) -> PositionStream<'str> {
        PositionStream {
            chars: source.chars(),
            peeked: None,
            cur_pos: Position::zero(),
        }
    }

    fn cur_pos(&self) -> Position {
        self.cur_pos.clone()
    }

    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

impl<'str> Iterator for PositionStream<'str> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.peeked.take() {
            next @ Some(_) => next,
            None => self.chars.next(),
        };

        match next {
            Some('\n') => self.cur_pos.new_line(),
            _ => self.cur_pos.add_col(),
        }
        next
    }
}

impl<'str> PeekingNext for PositionStream<'str> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_comment() -> Result<()> {
        let s = r#"
		//line
		"#;
        let stream = NoCommentsStream::from(s);
        let result: String = stream.try_collect()?;
        assert_eq!("\n\t\t\n\t\t", result);
        Ok(())
    }

    #[test]
    fn block_comment() -> Result<()> {
        let s = r#"
		/*
		block_comment
		*//**/
		/** block_comment **/
		"#;
        let stream = NoCommentsStream::from(s);
        let result: String = stream.try_collect()?;
        assert_eq!("\n\t\t\n\t\t\n\t\t", result);
        Ok(())
    }

    #[test]
    fn nested_block_comment() -> Result<()> {
        let s = r#"
		/*
		//line comment
		//line and */
		/*
		block_comment
		*//**/
		/** block_comment **/
		*/
		"#;
        let stream = NoCommentsStream::from(s);
        let result: String = stream.try_collect()?;
        assert_eq!("\n\t\t\n\t\t", result);
        Ok(())
    }

    #[test]
    fn no_comment_slash() -> Result<()> {
        let s = r#"5/5"#;
        let stream = NoCommentsStream::from(s);
        let result: String = stream.try_collect()?;
        assert_eq!("5/5", result);
        Ok(())
    }
}
