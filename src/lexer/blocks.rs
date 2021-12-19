use crate::error::{Error, Result};
use crate::lexer::{Delimeter, Token, TokenStream};
use itertools::{Itertools, PeekingNext};
use std::{fmt::Debug, iter::Peekable, vec::IntoIter};

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub tag: String,
    pub header: Vec<Token>,
    pub body: Vec<Block>,
}

enum TokenStream2<'str> {
    Stream(TokenStream<'str>),
    Vec(Peekable<IntoIter<Result<Token>>>),
}

impl<'str> Iterator for TokenStream2<'str> {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TokenStream2::Stream(stream) => stream.next(),
            TokenStream2::Vec(iter) => iter.next(),
        }
    }
}

impl<'str> PeekingNext for TokenStream2<'str> {
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
    where
        F: FnOnce(&Self::Item) -> bool,
    {
        match self {
            TokenStream2::Stream(stream) => stream.peeking_next(accept),
            TokenStream2::Vec(iter) => iter.peeking_next(accept),
        }
    }
}

impl<'str> TokenStream2<'str> {
    fn peek(&mut self) -> Option<&Result<Token>> {
        match self {
            TokenStream2::Stream(stream) => stream.peek(),
            TokenStream2::Vec(stream) => stream.peek(),
        }
    }
}

pub struct BlockStream<'str> {
    stream: TokenStream2<'str>,
    peeked: Option<Result<Block>>,
}

impl<'str> Iterator for BlockStream<'str> {
    type Item = Result<Block>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            next @ Some(_) => next,
            None => self.take_block(),
        }
    }
}

impl<'str> PeekingNext for BlockStream<'str> {
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

impl<'str> BlockStream<'str> {
    pub fn from(source: &'str str) -> BlockStream<'str> {
        BlockStream {
            stream: TokenStream2::Stream(TokenStream::from(source)),
            peeked: None,
        }
    }

    pub fn new(tokens: Vec<Token>) -> BlockStream<'str> {
        let tokens: Vec<_> = tokens.into_iter().map(Result::Ok).collect();
        BlockStream {
            stream: TokenStream2::Vec(tokens.into_iter().peekable()),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }

    fn take_block(&mut self) -> Option<Result<Block>> {
        loop {
            break match self.stream.next()? {
                Ok(Token::NewLine) => continue,
                Ok(Token::Ident(id)) => Some(self.collect_block(id)),
                Ok(token) => Some(Err(Error::syntax(
                    format!("Expected identifier: found {:?}", token),
                    0,
                )
                .into())),
                Err(e) => Some(Err(e.into())),
            };
        }
    }

    fn collect_block(&mut self, tag: String) -> Result<Block> {
        let header = self.collect_block_header()?;
        let body = self.collect_block_body()?;
        Ok(Block { tag, header, body })
    }

    fn collect_block_header(&mut self) -> Result<Vec<Token>> {
        let tokens = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(token) => !matches!(
                    token,
                    Token::Symbol(':') | Token::Symbol(';') | Token::Group(Delimeter::Braces, _)
                ),
                Err(_) => false,
            })
            .collect::<Result<Vec<_>>>();

        if let Some(Ok(Token::Symbol(':'))) = self.stream.peek() {
            self.stream.next();
        }
        tokens
    }

    fn collect_block_body(&mut self) -> Result<Vec<Block>> {
        let tokens = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(token) => !matches!(
                    token,
                    Token::Symbol(';') | Token::Group(Delimeter::Braces, _) | Token::NewLine
                ),
                Err(_) => false,
            })
            .collect::<Result<Vec<_>>>()?;

        let tokens = match self.stream.next() {
            Some(Ok(Token::Symbol(';'))) | None => Ok(tokens),
            Some(Ok(Token::Group(Delimeter::Braces, body_tokens))) => {
                assert!(tokens.is_empty());
                Ok(body_tokens)
            }
            Some(Ok(Token::NewLine)) => {
                Err(Error::syntax("Expected a ';' as end of one liner block".to_string(), 0).into())
            }
            Some(Err(e)) => Err(e),
            _ => Err(Error::syntax("Expected an end to block".to_string(), 0).into()),
        }?;

        BlockStream::new(tokens).collect()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::Value};

    #[test]
    fn blocks_test() -> Result<()> {
        let s = r#"	
			fn main() {
				fn function() {
					print "Line1";
					print "Line2";
					print "Line3";
				}
				fn f2(): print "one liner";
			
				call f2();
				call function();
			}
		"#;
        let blocks = BlockStream::from(s).collect::<Result<Vec<_>>>()?;
        assert_eq!(
			blocks,
			vec![Block { 
				tag: "fn".to_string(), 
				header: vec![Token::Ident("main".to_string()), Token::Group(Delimeter::Parens, vec![])], 
				body: vec![
                    Block {
                        tag: "fn".to_string(),
                        header: vec![Token::Ident("function".to_string()), Token::Group(Delimeter::Parens, vec![])],
                        body: vec![
                            Block {
                                tag: "print".to_string(),
                                header: vec![Token::Value(Value::String("Line1".to_string()))],
                                body: vec![]
                            },
                            Block {
                                tag: "print".to_string(),
                                header: vec![Token::Value(Value::String("Line2".to_string()))],
                                body: vec![]
                            },
                            Block {
                                tag: "print".to_string(),
                                header: vec![Token::Value(Value::String("Line3".to_string()))],
                                body: vec![]
                            },
                        ]
                    },
                    Block {
                        tag: "fn".to_string(),
                        header: vec![Token::Ident("f2".to_string()), Token::Group(Delimeter::Parens, vec![])],
                        body: vec![
                            Block {
                                tag: "print".to_string(),
                                header: vec![Token::Value(Value::String("one liner".to_string()))],
                                body: vec![]
                            },
                        ]
                    },
                    Block {
                        tag: "call".to_string(),
                        header: vec![Token::Ident("f2".to_string()), Token::Group(Delimeter::Parens, vec![])],
                        body: vec![]
                    },
                    Block {
                        tag: "call".to_string(),
                        header: vec![Token::Ident("function".to_string()), Token::Group(Delimeter::Parens, vec![])],
                        body: vec![]
                    },
				] 
			}]
		);
        Ok(())
    }
}