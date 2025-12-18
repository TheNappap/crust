mod block_collector;
mod block_tokens;
mod block_stream;

use crate::{lexer::{Span, Token}, utils::Result};
use std::{iter::{Map, Peekable}, vec::IntoIter};
use std::fmt::Debug;

pub use block_stream::{BlockStream};

type MapTokenResult = fn(Token) -> Result<Token>;
type FromVecStream = Peekable<Map<IntoIter<Token>, MapTokenResult>>;

fn from_vec_stream(tokens: Vec<Token>) -> FromVecStream {
    tokens.into_iter().map(Result::Ok as MapTokenResult).peekable()
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub tag: String,
    pub span: Span,
    pub header: Vec<Token>,
    pub body: Vec<Token>,
    pub chain: Option<Box<Block>>,
}

impl Block {
    pub fn is_anonymous(&self) -> bool {
        self.tag == ""
    }
    
    pub fn anonymous_block(header: Vec<Token>) -> Self {
        let span = header.iter().fold(None, |acc: Option<Span>, t| 
            match acc {
                Some(acc) => Some(acc.union(&t.span)),
                None => Some(t.span.clone()),
            }).expect("Can't make anonymous block from empty token list");
        Self { tag: "".into(), span, header, body: vec![], chain: None }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;
    use crate::{lexer::{Delimeter, Literal, Position, TokenKind}, utils::Result};

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
                    span: Span::new(Position::new(6, 0), Position::new(6, 35)),
                    header: vec![
                        Token{kind: TokenKind::Ident("f2".to_string()), span: Span::new(Position::new(6, 10), Position::new(6, 13))},
                        Token{kind: TokenKind::Group(Delimeter::Parens, vec![]), span: Span::new(Position::new(6, 13), Position::new(6, 15))},
                    ],
                    body: vec![
                        Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(6, 16), Position::new(6, 22))},
                        Token{kind: TokenKind::Literal(Literal::String("one liner".to_string())), span: Span::new(Position::new(6, 22), Position::new(6, 34))},
                        Token{kind: TokenKind::Semicolon, span: Span::new(Position::new(6, 34), Position::new(6, 35))},
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
                        span: Span::new(Position::new(2, 0), Position::new(2, 30)),
                        header: vec![],
                        body: vec![
                            Token{kind: TokenKind::Ident("print".to_string()), span: Span::new(Position::new(2, 13), Position::new(2, 19))},
                            Token{kind: TokenKind::Literal(Literal::String("Line1.1".to_string())), span: Span::new(Position::new(2, 19), Position::new(2, 29))},
                            Token{kind: TokenKind::Semicolon, span: Span::new(Position::new(2, 29), Position::new(2, 30))},
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
                        Token { kind: TokenKind::Range, span: Span::new(Position::new(10, 14), Position::new(10, 16)) }, 
                        Token { kind: TokenKind::Literal(Literal::Int(2)), span: Span::new(Position::new(10, 16), Position::new(10, 17)) },
                    ],
                    body: vec![],
                    chain: Some( Box::new(Block {
                        tag: "map".into(), 
                        span: Span::new(Position::new(10, 20), Position::new(10, 31)),
                        header: vec![Token { kind: TokenKind::Ident("x".into()), span: Span::new(Position::new(10, 24), Position::new(10, 26)) }],
                        body: vec![
                            Token { kind: TokenKind::Ident("x".into()), span: Span::new(Position::new(10, 27), Position::new(10, 29)) }, 
                            Token { kind: TokenKind::Plus, span: Span::new(Position::new(10, 29), Position::new(10, 30)) }, 
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
                                span: Span::new(Position::new(12, 0), Position::new(12, 48)), 
                                header: vec![Token { kind: TokenKind::Ident("z".into()), span: Span::new(Position::new(12, 23), Position::new(12, 25)) }], 
                                body: vec![
                                    Token { kind: TokenKind::Ident("call".into()), span: Span::new(Position::new(12, 26), Position::new(12, 31)) }, 
                                    Token { kind: TokenKind::Ident("do_something".into()), span: Span::new(Position::new(12, 31), Position::new(12, 44)) }, 
                                    Token { 
                                        kind: TokenKind::Group(Delimeter::Parens, vec![Token { kind: TokenKind::Ident("z".into()), span: Span::new(Position::new(12, 45), Position::new(12, 46)) }]),
                                        span: Span::new(Position::new(12, 44), Position::new(12, 47)) 
                                    },
                                    Token{kind: TokenKind::Semicolon, span: Span::new(Position::new(12, 47), Position::new(12, 48))},
                                ],
                                chain: None,
                            })),
                        })),
                    })),
                },
            ];
        for (block, test_block) in blocks.into_iter().zip(test_blocks.into_iter()) {
            assert_eq!( block, test_block);
        }
        Ok(())
    }
}
