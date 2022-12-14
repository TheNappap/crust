use crate::error::{Error, Result};
use crate::lexer::{Delimeter, Token, TokenStream, Operator};
use itertools::{Itertools, PeekingNext};
use std::{fmt::Debug, iter::Peekable, vec::IntoIter};

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub tag: String,
    pub header: Vec<Token>,
    pub body: Vec<Token>,
    pub chain: Option<Box<Block>>
}

impl Block {
    pub fn is_anonymous(&self) -> bool {
        self.tag == ""
    }
    
    pub fn anonymous_block(header: Vec<Token>) -> Block {
        Block { tag: "".into(), header, body: vec![], chain: None }
    }
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
        let tokens = tokens.into_iter()
            .map(Result::Ok)
            .collect::<Vec<_>>();
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
                Ok(literal @ Token::Literal(_)) => Some(Ok(Block::anonymous_block(vec![literal]))),
                Ok(group @ Token::Group(Delimeter::Braces, _)) => Some(Ok(Block::anonymous_block(vec![group]))),
                Ok(token) => Some(Err(Error::syntax(
                    format!("Expected identifier: found {:?}", token),
                    0,
                ))),
                Err(e) => Some(Err(e)),
            };
        }
    }

    fn collect_block(&mut self, tag: String) -> Result<Block> {
        if let Some(Ok(Token::Operator(Operator::Not))) = self.stream.peek() {
            self.stream.next();
        }

        let (header, header_token) = self.collect_block_header()?;
        let (body, chain) = self.collect_block_body_and_chain(header_token)?;
        Ok(Block { tag, header, body, chain })
    }

    fn collect_block_header(&mut self) -> Result<(Vec<Token>, Option<Token>)> {
        let tokens = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(token) => !matches!(
                    token,
                    Token::Operator(Operator::Colon | Operator::Eq | Operator::Semicolon) | Token::Group(Delimeter::Braces, _)
                ),
                Err(_) => true,
            })
            .filter(|token| {
                if let Ok(token) = token {
                    return *token != Token::NewLine;
                }
                true
            })
            .try_collect()?;

        if let Some(Ok(Token::Operator(Operator::Colon | Operator::Eq))) = self.stream.peek() {
            Ok((tokens, self.stream.next().transpose()?))
        } else {
            Ok((tokens, None))
        }
    }

    fn collect_block_body_and_chain(&mut self, header_token: Option<Token>) -> Result<(Vec<Token>, Option<Box<Block>>)> {
        let tokens: Vec<Token> = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(token) => !matches!(
                    token,
                    Token::Operator(Operator::Semicolon) | Token::Group(Delimeter::Braces, _) | Token::NewLine
                ),
                Err(_) => false,
            })
            .try_collect()?;

        let (tokens, chained_block) = match self.stream.next() {
            Some(Ok(Token::Operator(Operator::Semicolon))) | None => (tokens, None),
            Some(Ok(Token::NewLine)) => {
                (tokens, self.take_block().transpose()?)
            }
            Some(Ok(Token::Group(Delimeter::Braces, group_tokens))) => {
                let tokens = if header_token.is_some() {
                    let mut tokens = tokens;
                    tokens.push(Token::Group(Delimeter::Braces, group_tokens));
                    tokens
                } else { group_tokens };

                let chained = match self.stream.peek() {
                    Some(Ok(Token::NewLine)) => None,
                    _ => self.take_block().transpose()?,
                };
                (tokens, chained)
            }
            Some(Err(e)) => return Err(e),
            _ => return Err(Error::syntax("Expected an end to block".to_string(), 0)),
        };

        Ok((tokens, chained_block.map(Box::new)))
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::Literal};

    #[test]
    fn blocks_test() -> Result<()> {
        let s = r#"	
        fn function() {
            print "Line1"
            print "Line2"
            print "Line3"
        }
        fn f2(): print "one liner";
    
        call f2();
        call function();
        print "print statement"
		"#;
        let blocks: Vec<Block> = BlockStream::from(s).try_collect()?;
        assert_eq!(
            blocks,
            vec![
                Block {
                    tag: "fn".to_string(),
                    header: vec![
                        Token::Ident("function".to_string()),
                        Token::Group(Delimeter::Parens, vec![])
                    ],
                    body: vec![
                        Token::NewLine,
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("Line1".to_string())),
                        Token::NewLine,
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("Line2".to_string())),
                        Token::NewLine,
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("Line3".to_string())),
                        Token::NewLine,
                    ],
                    chain: None,
                },
                Block {
                    tag: "fn".to_string(),
                    header: vec![
                        Token::Ident("f2".to_string()),
                        Token::Group(Delimeter::Parens, vec![])
                    ],
                    body: vec![
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("one liner".to_string()))
                    ],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    header: vec![
                        Token::Ident("f2".to_string()),
                        Token::Group(Delimeter::Parens, vec![])
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    header: vec![
                        Token::Ident("function".to_string()),
                        Token::Group(Delimeter::Parens, vec![])
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "print".to_string(),
                    header: vec![Token::Literal(Literal::String("print statement".to_string()))],
                    body: vec![],
                    chain: None,
                },
            ]
        );
        Ok(())
    }

    #[test]
    fn chained_blocks_test() -> Result<()> {
        let s = r#"	
        if true: print "Line1.0"
        else: print "Line1.1";

        if false {
            print "Line2.0"
        } else {
            print "Line2.1"
        }
		"#;
        let blocks: Vec<Block> = BlockStream::from(s).try_collect()?;
        assert_eq!(
            blocks,
            vec![
                Block {
                    tag: "if".to_string(),
                    header: vec![Token::Ident("true".to_string())],
                    body: vec![
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("Line1.0".to_string())),
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        header: vec![],
                        body: vec![
                            Token::Ident("print".to_string()),
                            Token::Literal(Literal::String("Line1.1".to_string())),
                        ],
                        chain: None,
                    })),
                },
                Block {
                    tag: "if".to_string(),
                    header: vec![Token::Ident("false".to_string())],
                    body: vec![
                        Token::NewLine,
                        Token::Ident("print".to_string()),
                        Token::Literal(Literal::String("Line2.0".to_string())),
                        Token::NewLine,
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        header: vec![],
                        body: vec![
                            Token::NewLine,
                            Token::Ident("print".to_string()),
                            Token::Literal(Literal::String("Line2.1".to_string())),
                            Token::NewLine,
                        ],
                        chain: None,
                    })),
                },
            ],
        );
        Ok(())
    }
}
