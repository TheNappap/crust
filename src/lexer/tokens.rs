
use std::fmt;

use crate::error::{Result, ThrowablePosition};

use super::{no_comments::NoCommentsStream, span::Span};
use itertools::{Itertools, PeekingNext};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Delimeter {
    Braces,
    Parens,
    Brackets,
}

impl Delimeter {
    fn from(c: char) -> Option<Delimeter> {
        match c {
            '{' => Some(Delimeter::Braces),
            '(' => Some(Delimeter::Parens),
            '[' => Some(Delimeter::Brackets),
            _ => None,
        }
    }

    fn _start_char(&self) -> char {
        match self {
            Delimeter::Braces => '{',
            Delimeter::Parens => '(',
            Delimeter::Brackets => '[',
        }
    }

    fn end_char(&self) -> char {
        match self {
            Delimeter::Braces => '}',
            Delimeter::Parens => ')',
            Delimeter::Brackets => ']',
        }
    }
}

fn is_start_delimeter(c: char) -> bool {
    matches!(c, '{' | '(' | '[')
}

fn is_end_delimeter(c: char) -> bool {
    matches!(c, '}' | ')' | ']')
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Dot,
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    Arrow2,
    Plus,
    Dash,
    Star,
    Slash,
    Eq,
    EqEq,
    Neq,
    Not,
    Range,
}

#[derive(PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token({:#?}){{ {:?} }}", self.span.start(), self.kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Underscore,
    Ident(String),
    Literal(Literal),
    Operator(Operator),
    Symbol(char),
    Group(Delimeter, Vec<Token>),
    NewLine,
}

pub struct TokenStream<'str> {
    stream: NoCommentsStream<'str>,
    peeked: Option<Result<Token>>,
}

impl<'str> Iterator for TokenStream<'str> {
    type Item = Result<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            next @ Some(_) => next,
            None => self.take_token(),
        }
    }
}

impl<'str> PeekingNext for TokenStream<'str> {
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

impl<'str> TokenStream<'str> {
    pub fn from(source: &'str str) -> TokenStream<'str> {
        TokenStream {
            stream: NoCommentsStream::from(source),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.peeked.is_none() {
            self.peeked = self.next();
        }
        self.peeked.as_ref()
    }

    fn take_whitespace(&mut self) -> Result<()> {
        self.stream
            .peeking_take_while(|c| {
                c.as_ref()
                    .map_or(false, |&c| c.is_whitespace() && c != '\n')
            })
            .map_ok(|_| ())
            .collect()
    }

    fn take_alphanumeric_string(&mut self) -> Result<String> {
        self.stream
            .peeking_take_while(|c| {
                c.as_ref()
                    .map_or(false, |&c| c.is_alphanumeric() || c == '_')
            })
            .collect()
    }

    fn take_alphanumeric_token(&mut self) -> Result<TokenKind> {
        let string = self.take_alphanumeric_string()?;
        if string == "_" {
            return Ok(TokenKind::Underscore);
        }
        if !string.chars().all(|c| c.is_ascii_digit() || c == '_') {
            return Ok(TokenKind::Ident(string));
        }

        let is_dot = if let Some(Ok('.')) = self.stream.peek() { true } else { false };
        let is_second_dot = if let Some(Ok('.')) = self.stream.peek_second() { true } else { false };
        if is_dot && !is_second_dot {
            self.stream.next().transpose()?;
            let after_point = self.take_alphanumeric_string()?;
            if !after_point.chars().all(|c| c.is_ascii_digit() || c == '_') {
                return self.stream.cur_pos().lexer(r#"Expected number after '.'"#.into());
            }

            let num = string
                .chars()
                .filter(|c| c.is_ascii_digit())
                .chain(std::iter::once('.'))
                .chain(after_point.chars().filter(|&c| c.is_ascii_digit()))
                .collect::<String>()
                .parse::<f64>()
                .unwrap();
            Ok(TokenKind::Literal(Literal::Float(num)))
        } else {
            let num = string
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse::<i64>()
                .unwrap();
            Ok(TokenKind::Literal(Literal::Int(num)))
        }
    }

    fn take_string_token(&mut self) -> Result<TokenKind> {
        match self.stream.next().transpose()? {
            Some('\"') => (),
            _ => {
                return self.stream.cur_pos().lexer(r#"Expected start of string: '"'"#.into());
            }
        };
        let string: String = self
            .stream
            .peeking_take_while(|c| c.as_ref().map_or(false, |&c| c != '\"'))
            .try_collect()?;
        match self.stream.next().transpose()? {
            Some('\"') => (),
            _ => {
                return self.stream.cur_pos().lexer(r#"Expected end of string: '"'"#.into());
            }
        };
        Ok(TokenKind::Literal(Literal::String(string)))
    }

    fn take_group_token(&mut self, delimeter: Delimeter, start_span: Span) -> Result<Token> {
        let mut tokens = Vec::new();
        loop {
            match self.take_ungrouped_token().transpose()? {
                None => {
                    break self.stream.cur_pos().lexer(r#"Expected end delimeter"#.into())
                }
                Some(Token{ kind: TokenKind::Symbol(c), span: end_span }) if c == delimeter.end_char() => {
                    break Ok(Token{ kind: TokenKind::Group(delimeter, tokens), span: start_span.union(&end_span)})
                }
                Some(Token{ kind: TokenKind::Symbol(c), span }) if is_end_delimeter(c) => {
                    break span.lexer(
                        format!(
                            r#"Expected delimeter '{}', but found '{}'"#,
                            delimeter.end_char(),
                            c
                        )
                    )
                }
                Some(Token{ kind: TokenKind::Symbol(c), span }) if is_start_delimeter(c) => {
                    tokens.push(self.take_group_token(Delimeter::from(c).unwrap(), span)?)
                }
                Some(token) => tokens.push(token),
            }
        }
    }

    fn take_symbol(&mut self, c: char) -> Result<TokenKind> {
        let token = match c {
            '.' => match self.stream.peek() {
                Some(Ok('.')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::Range)
                }
                _ => TokenKind::Operator(Operator::Dot),
            }
            ',' => TokenKind::Operator(Operator::Comma),
            ':' => match self.stream.peek() {
                Some(Ok(':')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::ColonColon)
                }
                _ => TokenKind::Operator(Operator::Colon),
            },
            ';' => TokenKind::Operator(Operator::Semicolon),
            '+' => TokenKind::Operator(Operator::Plus),
            '-' => match self.stream.peek() {
                Some(Ok('>')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::Arrow)
                }
                _ => TokenKind::Operator(Operator::Dash),
            }
            '*' => TokenKind::Operator(Operator::Star),
            '/' => TokenKind::Operator(Operator::Slash),
            '=' => match self.stream.peek() {
                Some(Ok('=')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::EqEq)
                }
                Some(Ok('>')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::Arrow2)
                }
                _ => TokenKind::Operator(Operator::Eq)
            }
            '!' => match self.stream.peek() {
                Some(Ok('=')) => {
                    self.stream.next();
                    TokenKind::Operator(Operator::Neq)
                }
                _ => TokenKind::Operator(Operator::Not)
            }
            _ => TokenKind::Symbol(c)
        };
        Ok(token)
    }

    fn take_token(&mut self) -> Option<Result<Token>> {
        match self.take_ungrouped_token()? {
            Ok(Token{ kind: TokenKind::Symbol(c), span }) if is_end_delimeter(c) => Some(span.lexer(
                format!(r#"Unexpected end delimeter: '{}'"#, c),
            )),
            Ok(Token{ kind: TokenKind::Symbol(c), span }) if is_start_delimeter(c) => {
                Some(self.take_group_token(Delimeter::from(c).unwrap(), span))
            }
            result => Some(result),
        }
    }

    fn take_ungrouped_token(&mut self) -> Option<Result<Token>> {
        let start_pos = self.stream.cur_pos();
        if let Err(err) = self.take_whitespace() {
            return Some(Err(err));
        }
        let peek = match self.stream.peek()? {
            Ok(c) => *c,
            Err(err) => return Some(Err(err.clone())),
        };

        let token = match peek {
            c if c.is_alphanumeric() || c == '_' => self.take_alphanumeric_token(),
            c if c == '\"' => self.take_string_token(),
            _ => {
                let c = self.stream.next()?.unwrap();
                if c == '\n' {
                    Ok(TokenKind::NewLine)
                } else {
                    self.take_symbol(c)
                }
            }
        };
        let end_pos = self.stream.cur_pos();
        let span = Span::new(start_pos, end_pos);
        Some(token.map(|kind| Token { kind, span }))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Position;

    use super::Delimeter::*;
    use super::TokenKind::*;
    use super::Literal::*;
    use super::Operator::*;
    use super::*;

    #[test]
    fn let_statement() -> Result<()> {
        let s = r#"let v_a_r: u32 = 3_0+ 5_0. +8._9;
		"#;
        let tokens: Vec<Token> = TokenStream::from(s).try_collect()?;
        assert_eq!(
            tokens,
            vec![
                Token{ kind: Ident("let".into()), span: Span::new(Position::new(0,0),Position::new(0,3)) },
                Token{ kind: Ident("v_a_r".into()), span: Span::new(Position::new(0,3),Position::new(0,9)) },
                Token{ kind: Operator(Colon), span: Span::new(Position::new(0,9),Position::new(0,10)) },
                Token{ kind: Ident("u32".into()), span: Span::new(Position::new(0,10),Position::new(0,14)) },
                Token{ kind: Operator(Eq), span: Span::new(Position::new(0,14),Position::new(0,16)) },
                Token{ kind: Literal(Int(30)), span: Span::new(Position::new(0,16),Position::new(0,20)) },
                Token{ kind: Operator(Plus), span: Span::new(Position::new(0,20),Position::new(0,21)) },
                Token{ kind: Literal(Float(50.)), span: Span::new(Position::new(0,21),Position::new(0,26)) },
                Token{ kind: Operator(Plus), span: Span::new(Position::new(0,26),Position::new(0,28)) },
                Token{ kind: Literal(Float(8.9)), span: Span::new(Position::new(0,28),Position::new(0,32)) },
                Token{ kind: Operator(Semicolon), span: Span::new(Position::new(0,32),Position::new(0,33)) },
                Token{ kind: NewLine, span: Span::new(Position::new(0,33),Position::new(1,0)) },
            ]
        );
        Ok(())
    }

        #[test]
    fn range_statement() -> Result<()> {
        let s = r#"0..10"#;
        let tokens: Vec<Token> = TokenStream::from(s).try_collect()?;
        assert_eq!(
            tokens,
            vec![
                Token{ kind: Literal(Int(0)), span: Span::new(Position::new(0,0),Position::new(0,1)) },
                Token{ kind: Operator(Range), span: Span::new(Position::new(0,1),Position::new(0,3)) },
                Token{ kind: Literal(Int(10)), span: Span::new(Position::new(0,3),Position::new(0,10)) },
            ]
        );
        Ok(())
    }

    #[test]
    fn function_block() -> Result<()> {
        let s = r#"
		fn function(c: char) -> return {
			let strings = vec!["A String"];
		}
		"#;
        let tokens: Vec<Token> = TokenStream::from(s).try_collect()?;
        assert_eq!(
            tokens,
            vec![
                Token{ kind: NewLine, span: Span::new(Position::new(0,0),Position::new(1,0)) },
                Token{ kind: Ident("fn".into()), span: Span::new(Position::new(1,0),Position::new(1,4)) },
                Token{ kind: Ident("function".into()), span: Span::new(Position::new(1,4),Position::new(1,13)) },
                Token{ kind: Group(
                    Parens,
                    vec![
                        Token{ kind: Ident("c".into()), span: Span::new(Position::new(1,14),Position::new(1,15)) },
                        Token{ kind: Operator(Colon), span: Span::new(Position::new(1,15),Position::new(1,16)) },
                        Token{ kind: Ident("char".into()), span: Span::new(Position::new(1, 16),Position::new(1, 21)) },
                    ]
                ), span: Span::new(Position::new(1, 13),Position::new(1, 22)) },
                Token{ kind: Operator(Arrow), span: Span::new(Position::new(1, 22),Position::new(1, 25)) },
                Token{ kind: Ident("return".into()), span: Span::new(Position::new(1, 25),Position::new(1, 32)) },
                Token{ kind: Group(
                    Braces,
                    vec![
                        Token{ kind: NewLine, span: Span::new(Position::new(1, 34),Position::new(2, 0)) },
                        Token{ kind: Ident("let".into()), span: Span::new(Position::new(2, 0),Position::new(2, 6)) },
                        Token{ kind: Ident("strings".into()), span: Span::new(Position::new(2, 6),Position::new(2, 14)) },
                        Token{ kind: Operator(Eq), span: Span::new(Position::new(2, 14),Position::new(2, 16)) },
                        Token{ kind: Ident("vec".into()), span: Span::new(Position::new(2, 16),Position::new(2, 20)) },
                        Token{ kind: Operator(Not), span: Span::new(Position::new(2, 20),Position::new(2, 21)) },
                        Token{ kind: Group(Brackets, vec![
                            Token{ kind: Literal(String("A String".into())), span: Span::new(Position::new(2, 22),Position::new(2, 32)) }
                        ]), span: Span::new(Position::new(2, 21),Position::new(2, 33)) },
                        Token{ kind: Operator(Semicolon), span: Span::new(Position::new(2, 33),Position::new(2, 34)) },
                        Token{ kind: NewLine, span: Span::new(Position::new(2, 34),Position::new(3, 0)) },
                    ]
                ), span: Span::new(Position::new(1, 32),Position::new(3, 3)) },
                Token{ kind: NewLine, span: Span::new(Position::new(3, 3),Position::new(4, 0)) },
            ]
        );
        Ok(())
    }

    #[test]
    fn nested_block_test() -> Result<()> {
        let s = r#"	
			//line
			for x
			{
			2*2;
			while {
				let x = 5. + 5.6_0;
			}
			function_call();
			}
			"String}(]".into()[0];
			/**multiline!
			DAMN
			*/
			1_0 - 1;
			2_./1_0._1;
		"#;
        let tokens: Vec<Token> = TokenStream::from(s).try_collect()?;
        assert_eq!(
            tokens,
            vec![
                Token{ kind: NewLine, span: Span::new(Position::new(0, 0),Position::new(1, 0)) },
                Token{ kind: NewLine, span: Span::new(Position::new(1, 0),Position::new(3, 0)) },
                Token{ kind: Ident("for".into()), span: Span::new(Position::new(3, 0),Position::new(3, 6)) },
                Token{ kind: Ident("x".into()), span: Span::new(Position::new(3, 6),Position::new(3, 8)) },
                Token{ kind: NewLine, span: Span::new(Position::new(3, 8),Position::new(4, 0)) },
                Token{ kind: Group(
                    Braces,
                    vec![
                        Token{ kind: NewLine, span: Span::new(Position::new(4, 4),Position::new(5, 0)) },
                        Token{ kind: Literal(Int(2)), span: Span::new(Position::new(5, 0),Position::new(5, 4)) },
                        Token{ kind: Operator(Star), span: Span::new(Position::new(5, 4),Position::new(5, 5)) },
                        Token{ kind: Literal(Int(2)), span: Span::new(Position::new(5, 5),Position::new(5, 6)) },
                        Token{ kind: Operator(Semicolon), span: Span::new(Position::new(5, 6),Position::new(5, 7)) },
                        Token{ kind: NewLine, span: Span::new(Position::new(5, 7),Position::new(6, 0)) },
                        Token{ kind: Ident("while".into()), span: Span::new(Position::new(6, 0),Position::new(6, 8)) },
                        Token{ kind: Group(
                            Braces,
                            vec![
                                Token{ kind: NewLine, span: Span::new(Position::new(6, 10),Position::new(7, 0)) },
                                Token{ kind: Ident("let".into()), span: Span::new(Position::new(7, 0),Position::new(7, 7)) },
                                Token{ kind: Ident("x".into()), span: Span::new(Position::new(7, 7),Position::new(7, 9)) },
                                Token{ kind: Operator(Eq), span: Span::new(Position::new(7, 9),Position::new(7, 11)) },
                                Token{ kind: Literal(Float(5.)), span: Span::new(Position::new(7, 11),Position::new(7, 14)) },
                                Token{ kind: Operator(Plus), span: Span::new(Position::new(7, 14),Position::new(7, 16)) },
                                Token{ kind: Literal(Float(5.6)), span: Span::new(Position::new(7, 16),Position::new(7, 22)) },
                                Token{ kind: Operator(Semicolon), span: Span::new(Position::new(7, 22),Position::new(7, 23)) },
                                Token{ kind: NewLine, span: Span::new(Position::new(7, 23),Position::new(8, 0)) },
                            ]
                        ), span: Span::new(Position::new(6, 8),Position::new(8, 4)) },
                        Token{ kind: NewLine, span: Span::new(Position::new(8, 4),Position::new(9, 0)) },
                        Token{ kind: Ident("function_call".into()), span: Span::new(Position::new(9, 0),Position::new(9, 16)) },
                        Token{ kind: Group(Parens, vec![]), span: Span::new(Position::new(9, 16),Position::new(9, 18)) },
                        Token{ kind: Operator(Semicolon), span: Span::new(Position::new(9, 18),Position::new(9, 19)) },
                        Token{ kind: NewLine, span: Span::new(Position::new(9, 19),Position::new(10, 0)) },
                    ]
                ), span: Span::new(Position::new(4, 0),Position::new(10, 4)) },
                Token{ kind: NewLine, span: Span::new(Position::new(10, 4),Position::new(11, 0)) },
                Token{ kind: Literal(String("String}(]".into())), span: Span::new(Position::new(11, 0),Position::new(11, 14)) },
                Token{ kind: Operator(Dot), span: Span::new(Position::new(11, 14),Position::new(11, 15)) },
                Token{ kind: Ident("into".into()), span: Span::new(Position::new(11, 15),Position::new(11, 19)) },
                Token{ kind: Group(Parens, vec![]), span: Span::new(Position::new(11, 19),Position::new(11, 21)) },
                Token{ kind: Group(Brackets, vec![
                    Token{ kind: Literal(Int(0)), span: Span::new(Position::new(11, 22),Position::new(11, 23)) },
                ]), span: Span::new(Position::new(11, 21),Position::new(11, 24)) },
                Token{ kind: Operator(Semicolon), span: Span::new(Position::new(11, 24),Position::new(11, 25)) },
                Token{ kind: NewLine, span: Span::new(Position::new(11, 25),Position::new(12, 0)) },
                Token{ kind: NewLine, span: Span::new(Position::new(12, 0),Position::new(17, 0)) },
                Token{ kind: Literal(Int(10)), span: Span::new(Position::new(17, 0),Position::new(17, 6)) },
                Token{ kind: Operator(Dash), span: Span::new(Position::new(17, 6),Position::new(17, 8)) },
                Token{ kind: Literal(Int(1)), span: Span::new(Position::new(17, 8),Position::new(17, 10)) },
                Token{ kind: Operator(Semicolon), span: Span::new(Position::new(17, 10),Position::new(17, 11)) },
                Token{ kind: NewLine, span: Span::new(Position::new(17, 11),Position::new(18, 0)) },
                Token{ kind: Literal(Float(2.)), span: Span::new(Position::new(18, 0),Position::new(18, 6)) },
                Token{ kind: Operator(Slash), span: Span::new(Position::new(18, 6),Position::new(18, 8)) },
                Token{ kind: Literal(Float(10.1)), span: Span::new(Position::new(18, 8),Position::new(18, 14)) },
                Token{ kind: Operator(Semicolon), span: Span::new(Position::new(18, 14),Position::new(18, 15)) },
                Token{ kind: NewLine, span: Span::new(Position::new(18, 15),Position::new(19, 0)) },
            ]
        );
        Ok(())
    }
}
