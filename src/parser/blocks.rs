use crate::error::{Result, ThrowablePosition};
use crate::lexer::{Delimeter, Token, TokenStream, Operator, Span, TokenKind};
use itertools::{Itertools, PeekingNext};
use std::{fmt::Debug, iter::Peekable, vec::IntoIter};

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub tag: String,
    pub span: Span,
    pub header: Vec<Token>,
    pub body: Vec<Token>,
    pub chain: Option<Box<Block>>
}

impl Block {
    pub fn is_anonymous(&self) -> bool {
        self.tag == ""
    }
    
    pub fn anonymous_block(header: Vec<Token>) -> Block {
        let span = header.iter().fold(None, |acc: Option<Span>, t| 
            match acc {
                Some(acc) => Some(acc.union(&t.span)),
                None => Some(t.span.clone()),
            }).expect("Can't make anonymous block from empty token list");
        Block { tag: "".into(), span, header, body: vec![], chain: None }
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
            let token = match self.stream.next()? {
                Ok(token) => token,
                Err(e) => break Some(Err(e)),
            };

            break match token.kind {
                TokenKind::NewLine => continue,
                TokenKind::Ident(id) => Some(self.collect_block(id.to_owned(), token.span)),
                TokenKind::Literal(_) => Some(Ok(Block::anonymous_block(vec![token]))),
                TokenKind::Group(Delimeter::Braces, _) => Some(Ok(Block::anonymous_block(vec![token]))),
                token_kind => Some(token.span.syntax(
                    format!("Expected identifier: found {:?}", token_kind),
                )),
            };
        }
    }

    fn collect_block(&mut self, tag: String, tag_span: Span) -> Result<Block> {
        if let Some(Ok(Token{kind: TokenKind::Operator(Operator::Not), ..})) = self.stream.peek() {
            self.stream.next();
        }

        let (header, header_token) = self.collect_block_header()?;
        let (body, chain) = self.collect_block_body_and_chain(header_token)?;
        
        let span = match (&header[..], &body[..]) {
            ([..], [.., last]) => tag_span.union(&last.span),
            ([.., last], []) => tag_span.union(&last.span),
            ([], []) => tag_span,
        };
        Ok(Block { tag, span, header, body, chain })
    }

    fn collect_block_header(&mut self) -> Result<(Vec<Token>, Option<Token>)> {
        let tokens = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Operator(Operator::Colon | Operator::Eq | Operator::Semicolon) | TokenKind::Group(Delimeter::Braces, _)
                ),
                Err(_) => true,
            })
            .filter(|token| {
                if let Ok(Token{kind, ..}) = token {
                    return *kind != TokenKind::NewLine;
                }
                true
            })
            .try_collect()?;

        if let Some(Ok(Token{kind:TokenKind::Operator(Operator::Colon | Operator::Eq), ..})) = self.stream.peek() {
            Ok((tokens, self.stream.next().transpose()?))
        } else {
            Ok((tokens, None))
        }
    }

    fn collect_block_body_and_chain(&mut self, header_token: Option<Token>) -> Result<(Vec<Token>, Option<Box<Block>>)> {
        let tokens: Vec<Token> = self
            .stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Operator(Operator::Semicolon) | TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
                ),
                Err(_) => false,
            })
            .try_collect()?;

        let next_token = match self.stream.next() {
            None => return Ok((tokens, None)),
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e),
        };

        let (tokens, chained_block) = match &next_token.kind {
            TokenKind::Operator(Operator::Semicolon) => (tokens, None),
            TokenKind::NewLine => {
                (tokens, self.take_block().transpose()?)
            }
            TokenKind::Group(Delimeter::Braces, group_tokens) => {
                let tokens = if header_token.is_some() {
                    let mut tokens = tokens;
                    tokens.push(next_token);
                    tokens
                } else { group_tokens.clone() };

                let chained = match self.stream.peek() {
                    Some(Ok(Token{kind: TokenKind::NewLine, ..})) => None,
                    _ => self.take_block().transpose()?,
                };
                (tokens, chained)
            }
            _ => return next_token.span.syntax("Unexpected end of block".to_owned()),
        };

        Ok((tokens, chained_block.map(Box::new)))
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::{Literal, Position}};

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
                    span: Span::new(Position::new(1, 11), Position::new(5, 0)),
                    header: vec![
                        Token{kind: TokenKind::Ident("function".to_string()), span: Span::new(Position::new(1, 11), Position::new(1, 20))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(1, 20), Position::new(1, 21))},
                    ],
                    body: vec![
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(1, 23), Position::new(2, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(2, 0), Position::new(2, 18))},
                        Token{kind: TokenKind::Literal(Literal::String("Line1".to_string())), span: Span::new(Position::new(2, 18), Position::new(2, 25))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(2, 25), Position::new(3, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(3, 0), Position::new(3, 18))},
                        Token{kind: TokenKind::Literal(Literal::String("Line2".to_string())), span: Span::new(Position::new(3, 18), Position::new(3, 25))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(3, 25), Position::new(4, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(4, 0), Position::new(4, 18))},
                        Token{kind: TokenKind::Literal(Literal::String("Line3".to_string())), span: Span::new(Position::new(4, 18), Position::new(4, 25))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(4, 25), Position::new(5, 0))},
                    ],
                    chain: None,
                },
                Block {
                    tag: "fn".to_string(),
                    span: Span::new(Position::new(6, 11), Position::new(6, 34)),
                    header: vec![
                        Token{kind: TokenKind::Ident("f2".to_string()), span: Span::new(Position::new(6, 11), Position::new(6, 14))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(6, 14), Position::new(6, 15))},
                    ],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(6, 17), Position::new(6, 23))},
                        Token{kind: TokenKind::Literal(Literal::String("one liner".to_string())), span: Span::new(Position::new(6, 23), Position::new(6, 34))},
                    ],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    span: Span::new(Position::new(8, 13), Position::new(8, 17)),
                    header: vec![
                        Token{kind: TokenKind::Ident("f2".to_string()), span: Span::new(Position::new(8, 13), Position::new(8, 16))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(8, 16), Position::new(8, 17))},
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    span: Span::new(Position::new(9, 13), Position::new(9, 23)),
                    header: vec![
                        Token{kind: TokenKind::Ident("function".to_string()), span: Span::new(Position::new(9, 13), Position::new(9, 22))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(9, 22), Position::new(9, 23))},
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "print".to_string(),
                    span: Span::new(Position::new(10, 14), Position::new(10, 31)),
                    header: vec![Token{kind: TokenKind::Literal(Literal::String("print statement".to_string())), span: Span::new(Position::new(10, 14), Position::new(10, 31))}],
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
                    span: Span::new(Position::new(1, 11), Position::new(1, 32)),
                    header: vec![Token{kind: TokenKind::Ident("true".to_string()), span: Span::new(Position::new(1, 11), Position::new(1, 16))}],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(1, 17), Position::new(1, 23))},
                        Token{kind: TokenKind::Literal(Literal::String("Line1.0".to_string())), span: Span::new(Position::new(1, 23), Position::new(1, 32))},
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        span: Span::new(Position::new(2, 14), Position::new(2, 29)),
                        header: vec![],
                        body: vec![
                            Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(2, 14), Position::new(2, 20))},
                            Token{kind: TokenKind::Literal(Literal::String("Line1.1".to_string())), span: Span::new(Position::new(2, 20), Position::new(2, 29))},
                        ],
                        chain: None,
                    })),
                },
                Block {
                    tag: "if".to_string(),
                    span: Span::new(Position::new(4, 11), Position::new(6, 0)),
                    header: vec![Token{kind: TokenKind::Ident("false".to_string()), span: Span::new(Position::new(4, 11), Position::new(4, 17))}],
                    body: vec![
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(4, 18), Position::new(5, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(5, 0), Position::new(5, 18))},
                        Token{kind: TokenKind::Literal(Literal::String("Line2.0".to_string())), span: Span::new(Position::new(5, 18), Position::new(5, 27))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(5, 27), Position::new(6, 0))},
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        span: Span::new(Position::new(6, 16), Position::new(8, 0)),
                        header: vec![],
                        body: vec![
                            Token{kind: TokenKind::NewLine, span: Span::new(Position::new(6, 16), Position::new(7, 0))},
                            Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(7, 0), Position::new(7, 18))},
                            Token{kind: TokenKind::Literal(Literal::String("Line2.1".to_string())), span: Span::new(Position::new(7, 18), Position::new(7, 27))},
                            Token{kind: TokenKind::NewLine, span: Span::new(Position::new(7, 27), Position::new(8, 0))},
                        ],
                        chain: None,
                    })),
                },
            ],
        );
        Ok(())
    }
}
