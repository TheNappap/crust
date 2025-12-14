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

trait Peeking : PeekingNext {
    fn peek(&mut self) -> Option<&<Self as Iterator>::Item>;
}

impl<I: Iterator> Peeking for Peekable<I> {
    fn peek(&mut self) -> Option<&I::Item> {
        self.peek()
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

impl<'str> Peeking for TokenStream2<'str> {
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
            None => BlockStream::take_block(&mut self.stream),
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

    fn take_block<S>(stream: &mut S) -> Option<Result<Block>> where S: Peeking<Item=Result<Token>> {
        loop {
            let token = match stream.next()? {
                Ok(token) => token,
                Err(e) => break Some(Err(e)),
            };

            break match token.kind {
                TokenKind::NewLine | TokenKind::Operator(Operator::Arrow2) => continue,
                TokenKind::Ident(id) => Some(BlockStream::collect_block(stream, id.to_owned(), token.span)),
                TokenKind::Literal(_) => Some(Ok(Block::anonymous_block(vec![token]))),
                TokenKind::Group(_, _) => Some(Ok(Block::anonymous_block(vec![token]))),
                token_kind => Some(token.span.syntax(
                    format!("Expected identifier: found {:?}", token_kind),
                )),
            };
        }
    }

    fn collect_block<S>(stream: &mut S, tag: String, tag_span: Span) -> Result<Block> where S: Peeking<Item=Result<Token>> {
        if let Some(Ok(Token{kind: TokenKind::Operator(Operator::Not), ..})) = stream.peek() {
            stream.next();
        }

        let (header, header_token) = BlockStream::collect_block_header(stream)?;
        let (body, chain) = if let Some(Token{ kind:TokenKind::Operator(Operator::Eq), .. }) = header_token {
            (BlockStream::collect_block_tokens(stream)?, None)
        } else {
            let (body, chain) = BlockStream::collect_body_and_chain(stream, header_token)?;
            let chain = chain.map(|tokens| {
                let mut peekable = tokens.into_iter().map(Result::Ok).peekable();
                let block = BlockStream::take_block(&mut peekable)?;
                Some(block.map(Box::new))
            }).flatten().transpose()?;
            (body, chain)
        };
        
        let span = match (&header[..], &body[..]) {
            ([..], [.., last]) => tag_span.union(&last.span),
            ([.., last], []) => tag_span.union(&last.span),
            ([], []) => tag_span,
        };
        Ok(Block { tag, span, header, body, chain })
    }

    fn collect_block_header<S>(stream: &mut S) -> Result<(Vec<Token>, Option<Token>)> where S: Peeking<Item=Result<Token>> {
        let tokens = stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Operator(Operator::Colon | Operator::Eq | Operator::Arrow2 | Operator::Semicolon) | TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
                ),
                Err(_) => true,
            })
            .try_collect()?;

        if let Some(Ok(Token{kind:TokenKind::Operator(Operator::Colon | Operator::Eq), ..})) = stream.peek() {
            Ok((tokens, stream.next().transpose()?))
        } else {
            Ok((tokens, None))
        }
    }
    
    fn collect_block_tokens<S>(stream: &mut S) -> Result<Vec<Token>> where S: Peeking<Item=Result<Token>> {
        let mut ends_on_braces = false;
        let mut tokens: Vec<_> = stream
            .take_while_inclusive(|token| match token {
                Ok(Token{kind:TokenKind::Operator(Operator::Semicolon), ..}) => false,
                Ok(Token{kind:TokenKind::Group(Delimeter::Braces, _), ..}) => {
                    ends_on_braces = true;
                    false
                },
                Err(_) => false,
                _ => true
            })
            .try_collect()?;

        match stream.peek() {
            _ if !ends_on_braces => {}
            Some(Ok(Token{kind:TokenKind::NewLine, ..})) => {}
            _ => {
                let extra_tokens = BlockStream::collect_block_tokens(stream)?;
                tokens.extend(extra_tokens);
            }
        }
        Ok(tokens)
    }

    fn collect_body_and_chain<S>(stream: &mut S, header_token: Option<Token>) -> Result<(Vec<Token>, Option<Vec<Token>>)> where S: Peeking<Item=Result<Token>> {
        let tokens: Vec<Token> = stream
            .peeking_take_while(|token| match token {
                Ok(Token{kind, ..}) => !matches!(
                    kind,
                    TokenKind::Operator(Operator::Semicolon | Operator::Arrow2) | TokenKind::Group(Delimeter::Braces, _) | TokenKind::NewLine
                ),
                Err(_) => false,
            })
            .try_collect()?;

        let end_token = match stream.next() {
            None => return Ok((tokens, None)),
            Some(Err(e)) => return Err(e),
            Some(Ok(token)) => token,
        };

        let tokens = match &end_token.kind {
            TokenKind::Operator(Operator::Semicolon) => return Ok((tokens, None)),
            TokenKind::NewLine | TokenKind::Operator(Operator::Arrow2) => {
                tokens
            }
            TokenKind::Group(Delimeter::Braces, group_tokens) => {
                if header_token.is_some() {
                    let mut tokens = tokens;
                    tokens.push(end_token);
                    tokens
                } else {
                    assert!(tokens.is_empty());
                    group_tokens.clone()
                }
            }
            _ => return end_token.span.syntax("Unexpected end of block".to_owned()),
        };

        match stream.peek() {
            None | Some(Ok(Token{kind:TokenKind::NewLine, ..})) => return Ok((tokens, None)),
            Some(Err(e)) => return Err(e.clone()),
            Some(Ok(_)) => (),
        };

        Ok((tokens, Some(BlockStream::collect_block_tokens(stream)?)))
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
        let test_blocks = vec![
                Block {
                    tag: "fn".to_string(),
                    span: Span::new(Position::new(1, 0), Position::new(4, 25)),
                    header: vec![
                        Token{kind: TokenKind::Ident("function".to_string()), span: Span::new(Position::new(1, 10), Position::new(1, 19))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(1, 19), Position::new(1, 21))},
                    ],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(2, 0), Position::new(2, 17))},
                        Token{kind: TokenKind::Literal(Literal::String("Line1".to_string())), span: Span::new(Position::new(2, 17), Position::new(2, 25))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(2, 25), Position::new(3, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(3, 0), Position::new(3, 17))},
                        Token{kind: TokenKind::Literal(Literal::String("Line2".to_string())), span: Span::new(Position::new(3, 17), Position::new(3, 25))},
                        Token{kind: TokenKind::NewLine, span: Span::new(Position::new(3, 25), Position::new(4, 0))},
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(4, 0), Position::new(4, 17))},
                        Token{kind: TokenKind::Literal(Literal::String("Line3".to_string())), span: Span::new(Position::new(4, 17), Position::new(4, 25))},
                    ],
                    chain: None,
                },
                Block {
                    tag: "fn".to_string(),
                    span: Span::new(Position::new(6, 0), Position::new(6, 34)),
                    header: vec![
                        Token{kind: TokenKind::Ident("f2".to_string()), span: Span::new(Position::new(6, 10), Position::new(6, 13))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(6, 13), Position::new(6, 15))},
                    ],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(6, 16), Position::new(6, 22))},
                        Token{kind: TokenKind::Literal(Literal::String("one liner".to_string())), span: Span::new(Position::new(6, 22), Position::new(6, 34))},
                    ],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    span: Span::new(Position::new(8, 0), Position::new(8, 17)),
                    header: vec![
                        Token{kind: TokenKind::Ident("f2".to_string()), span: Span::new(Position::new(8, 12), Position::new(8, 15))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(8, 15), Position::new(8, 17))},
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "call".to_string(),
                    span: Span::new(Position::new(9, 0), Position::new(9, 23)),
                    header: vec![
                        Token{kind: TokenKind::Ident("function".to_string()), span: Span::new(Position::new(9, 12), Position::new(9, 21))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(9, 21), Position::new(9, 23))},
                    ],
                    body: vec![],
                    chain: None,
                },
                Block {
                    tag: "print".to_string(),
                    span: Span::new(Position::new(10, 0), Position::new(10, 31)),
                    header: vec![Token{kind: TokenKind::Literal(Literal::String("print statement".to_string())), span: Span::new(Position::new(10, 13), Position::new(10, 31))}],
                    body: vec![],
                    chain: None,
                },
            ];
        for (block, test_block) in blocks.into_iter().zip(test_blocks.into_iter()) {
            assert_eq!( block, test_block);
        }
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

        iter 0..2 => map x: x+1
                    filter y: y < 3
                    for z: call do_something(z);
		"#;
        let blocks: Vec<Block> = BlockStream::from(s).try_collect()?;
        let test_blocks = vec![
                Block {
                    tag: "if".to_string(),
                    span: Span::new(Position::new(1, 0), Position::new(1, 32)),
                    header: vec![Token{kind: TokenKind::Ident("true".to_string()), span: Span::new(Position::new(1, 10), Position::new(1, 15))}],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(1, 16), Position::new(1, 22))},
                        Token{kind: TokenKind::Literal(Literal::String("Line1.0".to_string())), span: Span::new(Position::new(1, 22), Position::new(1, 32))},
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        span: Span::new(Position::new(2, 0), Position::new(2, 29)),
                        header: vec![],
                        body: vec![
                            Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(2, 13), Position::new(2, 19))},
                            Token{kind: TokenKind::Literal(Literal::String("Line1.1".to_string())), span: Span::new(Position::new(2, 19), Position::new(2, 29))},
                        ],
                        chain: None,
                    })),
                },
                Block {
                    tag: "if".to_string(),
                    span: Span::new(Position::new(4, 0), Position::new(5, 27)),
                    header: vec![Token{kind: TokenKind::Ident("false".to_string()), span: Span::new(Position::new(4, 10), Position::new(4, 16))}],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(5, 0), Position::new(5, 17))},
                        Token{kind: TokenKind::Literal(Literal::String("Line2.0".to_string())), span: Span::new(Position::new(5, 17), Position::new(5, 27))},
                    ],
                    chain: Some(Box::new(Block {
                        tag: "else".to_string(),
                        span: Span::new(Position::new(6, 9), Position::new(7, 27)),
                        header: vec![],
                        body: vec![
                            Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(7, 0), Position::new(7, 17))},
                            Token{kind: TokenKind::Literal(Literal::String("Line2.1".to_string())), span: Span::new(Position::new(7, 17), Position::new(7, 27))},
                        ],
                        chain: None,
                    })),
                },
                Block {
                    tag: "iter".to_string(),
                    span: Span::new(Position::new(10, 0), Position::new(10, 17)),
                    header: vec![
                        Token { kind: TokenKind::Literal(Literal::Int(0)), span: Span::new(Position::new(10, 12), Position::new(10, 14)) },
                        Token { kind: TokenKind::Operator(Operator::Range), span: Span::new(Position::new(10, 14), Position::new(10, 16)) }, 
                        Token { kind: TokenKind::Literal(Literal::Int(2)), span: Span::new(Position::new(10, 16), Position::new(10, 17)) },
                    ],
                    body: vec![],
                    chain: Some( Box::new(Block {
                        tag: "map".into(), 
                        span: Span::new(Position::new(10, 20), Position::new(10, 31)),
                        header: vec![Token { kind: TokenKind::Ident("x".into()), span: Span::new(Position::new(10, 24), Position::new(10, 26)) }],
                        body: vec![
                            Token { kind: TokenKind::Ident("x".into()), span: Span::new(Position::new(10, 27), Position::new(10, 29)) }, 
                            Token { kind: TokenKind::Operator(Operator::Plus), span: Span::new(Position::new(10, 29), Position::new(10, 30)) }, 
                            Token { kind: TokenKind::Literal(Literal::Int(1)), span: Span::new(Position::new(10, 30), Position::new(10, 31)) }
                        ],
                        chain: Some( Box::new(Block { 
                            tag: "filter".into(),
                            span: Span::new(Position::new(11, 0), Position::new(11, 35)), 
                            header: vec![Token { kind: TokenKind::Ident("y".into()), span: Span::new(Position::new(11, 26), Position::new(11, 28)) }], 
                            body: vec![
                                Token { kind: TokenKind::Ident("y".into()), span: Span::new(Position::new(11, 29), Position::new(11, 31)) }, 
                                Token { kind: TokenKind::Symbol('<'), span: Span::new(Position::new(11, 31), Position::new(11, 33)) }, 
                                Token { kind: TokenKind::Literal(Literal::Int(3)), span: Span::new(Position::new(11, 33), Position::new(11, 35)) }
                            ],
                            chain: Some( Box::new(Block { 
                                tag: "for".into(), 
                                span: Span::new(Position::new(12, 0), Position::new(12, 47)), 
                                header: vec![Token { kind: TokenKind::Ident("z".into()), span: Span::new(Position::new(12, 23), Position::new(12, 25)) }], 
                                body: vec![
                                    Token { kind: TokenKind::Ident("call".into()), span: Span::new(Position::new(12, 26), Position::new(12, 31)) }, 
                                    Token { kind: TokenKind::Ident("do_something".into()), span: Span::new(Position::new(12, 31), Position::new(12, 44)) }, 
                                    Token { 
                                        kind: TokenKind::Group(Delimeter::Parens, vec![Token { kind: TokenKind::Ident("z".into()), span: Span::new(Position::new(12, 45), Position::new(12, 46)) }]),
                                        span: Span::new(Position::new(12, 44), Position::new(12, 47)) 
                                    }], 
                                chain: None
                            }))
                        }))
                    }))
                },
            ];
        for (block, test_block) in blocks.into_iter().zip(test_blocks.into_iter()) {
            assert_eq!( block, test_block);
        }
        Ok(())
    }
}
