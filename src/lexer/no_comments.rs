use itertools::{Itertools, PeekingNext};
use std::str::Chars;

use crate::error::{Error, Result};

pub struct NoCommentsStream<'str> {
    stream: LineCount<'str>,
    peeked: Option<Result<char>>,
}

impl<'str> NoCommentsStream<'str> {
    pub fn from(source: &'str str) -> NoCommentsStream<'str> {
        NoCommentsStream {
            stream: LineCount::from(source),
            peeked: None,
        }
    }

    pub fn cur_line(&self) -> usize {
        self.stream.cur_line()
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
                (None, _) => return Err(Error::lexer(r#"Expected '*/'"#.into(), self.cur_line())),
                _ => (),
            }
        }
    }
}

struct LineCount<'str> {
    chars: Chars<'str>,
    peeked: Option<char>,
    cur_line: usize,
}

impl<'str> LineCount<'str> {
    fn from(source: &'str str) -> LineCount<'str> {
        LineCount {
            chars: source.chars(),
            peeked: None,
            cur_line: 0,
        }
    }

    fn cur_line(&self) -> usize {
        match self.peeked {
            Some('\n') => self.cur_line - 1,
            _ => self.cur_line,
        }
    }

    fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }
}

impl<'str> Iterator for LineCount<'str> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next = match self.peeked.take() {
            next @ Some(_) => next,
            None => self.chars.next(),
        };

        match next {
            next @ Some('\n') => {
                self.cur_line += 1;
                next
            }
            next => next,
        }
    }
}

impl<'str> PeekingNext for LineCount<'str> {
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
        let result: String = stream.collect::<Result<_>>()?;
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
        let result: String = stream.collect::<Result<_>>()?;
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
        let result: String = stream.collect::<Result<_>>()?;
        assert_eq!("\n\t\t\n\t\t", result);
        Ok(())
    }

    #[test]
    fn no_comment_slash() -> Result<()> {
        let s = r#"5/5"#;
        let stream = NoCommentsStream::from(s);
        let result: String = stream.collect::<Result<_>>()?;
        assert_eq!("5/5", result);
        Ok(())
    }
}
