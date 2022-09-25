use crate::error::{Error, Result};

use super::no_comments::NoCommentsStream;
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
    Semicolon,
    Assign,
    Excl,
    Arrow,
    Arrow2,
    Plus,
    Dash,
    Star,
    Slash,
    Eq,
    Neq
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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

    pub fn cur_line(&self) -> usize {
        self.stream.cur_line()
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

    fn take_alphanumeric_token(&mut self) -> Result<Token> {
        let string = self.take_alphanumeric_string()?;
        if !string.chars().all(|c| c.is_ascii_digit() || c == '_') {
            return Ok(Token::Ident(string));
        }

        if let Some(Ok('.')) = self.stream.peek() {
            self.stream.next().transpose()?;
            let after_point = self.take_alphanumeric_string()?;
            if !after_point.chars().all(|c| c.is_ascii_digit() || c == '_') {
                return Err(Error::lexer(
                    r#"Expected number after '.'"#.into(),
                    self.cur_line(),
                ));
            }

            let num = string
                .chars()
                .filter(|c| c.is_ascii_digit())
                .chain(std::iter::once('.'))
                .chain(after_point.chars().filter(|&c| c.is_ascii_digit()))
                .collect::<String>()
                .parse::<f64>()
                .unwrap();
            Ok(Token::Literal(Literal::Float(num)))
        } else {
            let num = string
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse::<i64>()
                .unwrap();
            Ok(Token::Literal(Literal::Int(num)))
        }
    }

    fn take_string_token(&mut self) -> Result<Token> {
        match self.stream.next().transpose()? {
            Some('\"') => (),
            _ => {
                return Err(Error::lexer(
                    r#"Expected start of string: '"'"#.into(),
                    self.cur_line(),
                ))
            }
        };
        let string = self
            .stream
            .peeking_take_while(|c| c.as_ref().map_or(false, |&c| c != '\"'))
            .collect::<Result<String>>()?;
        match self.stream.next().transpose()? {
            Some('\"') => (),
            _ => {
                return Err(Error::lexer(
                    r#"Expected end of string: '"'"#.into(),
                    self.cur_line(),
                ))
            }
        };
        Ok(Token::Literal(Literal::String(string)))
    }

    fn take_group_token(&mut self, delimeter: Delimeter) -> Result<Token> {
        let mut tokens = Vec::new();
        loop {
            match self.take_ungrouped_token().transpose()? {
                None => {
                    break Err(Error::lexer(
                        r#"Expected end delimeter"#.into(),
                        self.cur_line(),
                    ))
                }
                Some(Token::Symbol(c)) if c == delimeter.end_char() => {
                    break Ok(Token::Group(delimeter, tokens))
                }
                Some(Token::Symbol(c)) if is_end_delimeter(c) => {
                    break Err(Error::lexer(
                        format!(
                            r#"Expected delimeter '{}', but found '{}'"#,
                            delimeter.end_char(),
                            c
                        ),
                        self.cur_line(),
                    ))
                }
                Some(Token::Symbol(c)) if is_start_delimeter(c) => {
                    tokens.push(self.take_group_token(Delimeter::from(c).unwrap())?)
                }
                Some(token) => tokens.push(token),
            }
        }
    }

    fn take_symbol(&mut self, c: char) -> Result<Token> {
        let token = match c {
            '.' => Token::Operator(Operator::Dot),
            ',' => Token::Operator(Operator::Comma),
            ':' => Token::Operator(Operator::Colon),
            ';' => Token::Operator(Operator::Semicolon),
            '+' => Token::Operator(Operator::Plus),
            '-' => match self.stream.peek() {
                Some(Ok('>')) => {
                    self.stream.next();
                    Token::Operator(Operator::Arrow)
                }
                _ => Token::Operator(Operator::Dash),
            }
            '*' => Token::Operator(Operator::Star),
            '/' => Token::Operator(Operator::Slash),
            '=' => match self.stream.peek() {
                Some(Ok('=')) => {
                    self.stream.next();
                    Token::Operator(Operator::Eq)
                }
                Some(Ok('>')) => {
                    self.stream.next();
                    Token::Operator(Operator::Arrow2)
                }
                _ => Token::Operator(Operator::Assign)
            }
            '!' => match self.stream.peek() {
                Some(Ok('=')) => {
                    self.stream.next();
                    Token::Operator(Operator::Neq)
                }
                _ => Token::Operator(Operator::Excl)
            }
            _ => Token::Symbol(c)
        };
        Ok(token)
    }

    fn take_token(&mut self) -> Option<Result<Token>> {
        match self.take_ungrouped_token()? {
            Ok(Token::Symbol(c)) if is_end_delimeter(c) => Some(Err(Error::lexer(
                format!(r#"Unexpected end delimeter: '{}'"#, c),
                self.cur_line(),
            ))),
            Ok(Token::Symbol(c)) if is_start_delimeter(c) => {
                Some(self.take_group_token(Delimeter::from(c).unwrap()))
            }
            result => Some(result),
        }
    }

    fn take_ungrouped_token(&mut self) -> Option<Result<Token>> {
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
                    Ok(Token::NewLine)
                } else {
                    self.take_symbol(c)
                }
            }
        };
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::Delimeter::*;
    use super::Token::*;
    use super::Literal::*;
    use super::Operator::*;
    use super::*;

    #[test]
    fn let_statement() -> Result<()> {
        let s = r#"let v_a_r: u32 = 3_0+ 5_0. +8._9;
		"#;
        let list: Result<Vec<_>> = TokenStream::from(s).collect();
        assert_eq!(
            list?,
            vec![
                Ident("let".into()),
                Ident("v_a_r".into()),
                Operator(Colon),
                Ident("u32".into()),
                Operator(Assign),
                Literal(Int(30)),
                Operator(Plus),
                Literal(Float(50.)),
                Operator(Plus),
                Literal(Float(8.9)),
                Operator(Semicolon),
                NewLine
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
        let list: Result<Vec<_>> = TokenStream::from(s).collect();
        assert_eq!(
            list?,
            vec![
                NewLine,
                Ident("fn".into()),
                Ident("function".into()),
                Group(
                    Parens,
                    vec![Ident("c".into()), Operator(Colon), Ident("char".into())]
                ),
                Operator(Arrow),
                Ident("return".into()),
                Group(
                    Braces,
                    vec![
                        NewLine,
                        Ident("let".into()),
                        Ident("strings".into()),
                        Operator(Assign),
                        Ident("vec".into()),
                        Operator(Excl),
                        Group(Brackets, vec![Literal(String("A String".into()))]),
                        Operator(Semicolon),
                        NewLine
                    ]
                ),
                NewLine
            ]
        );
        Ok(())
    }

    #[test]
    fn tokenize_test() -> Result<()> {
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
        let tokens = TokenStream::from(s).collect::<Result<Vec<_>>>()?;
        assert_eq!(
            tokens,
            vec![
                NewLine,
                NewLine,
                Ident("for".into()),
                Ident("x".into()),
                NewLine,
                Group(
                    Braces,
                    vec![
                        NewLine,
                        Literal(Int(2)),
                        Operator(Star),
                        Literal(Int(2)),
                        Operator(Semicolon),
                        NewLine,
                        Ident("while".into()),
                        Group(
                            Braces,
                            vec![
                                NewLine,
                                Ident("let".into()),
                                Ident("x".into()),
                                Operator(Assign),
                                Literal(Float(5.)),
                                Operator(Plus),
                                Literal(Float(5.6)),
                                Operator(Semicolon),
                                NewLine
                            ]
                        ),
                        NewLine,
                        Ident("function_call".into()),
                        Group(Parens, vec![]),
                        Operator(Semicolon),
                        NewLine
                    ]
                ),
                NewLine,
                Literal(String("String}(]".into())),
                Operator(Dot),
                Ident("into".into()),
                Group(Parens, vec![]),
                Group(Brackets, vec![Literal(Int(0))]),
                Operator(Semicolon),
                NewLine,
                NewLine,
                Literal(Int(10)),
                Operator(Dash),
                Literal(Int(1)),
                Operator(Semicolon),
                NewLine,
                Literal(Float(2.)),
                Operator(Slash),
                Literal(Float(10.1)),
                Operator(Semicolon),
                NewLine
            ]
        );
        Ok(())
    }
}
