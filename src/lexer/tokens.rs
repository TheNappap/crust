use crate::error::{Error, Result};

use super::no_comments::NoCommentsStream;
use itertools::{Itertools, PeekingNext};

#[derive(Debug, PartialEq, Clone)]
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
pub enum Value {
    Int(i32),
    Float(f64),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    Value(Value),
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
            Ok(Token::Value(Value::Float(num)))
        } else {
            let num = string
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse::<i32>()
                .unwrap();
            Ok(Token::Value(Value::Int(num)))
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
        Ok(Token::Value(Value::String(string)))
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
            c => {
                if let Some(Err(err)) = self.stream.next() {
                    return Some(Err(err));
                }
                if c == '\n' {
                    Ok(Token::NewLine)
                } else {
                    Ok(Token::Symbol(c))
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
    use super::Value::*;
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
                Symbol(':'),
                Ident("u32".into()),
                Symbol('='),
                Value(Int(30)),
                Symbol('+'),
                Value(Float(50.)),
                Symbol('+'),
                Value(Float(8.9)),
                Symbol(';'),
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
                    vec![Ident("c".into()), Symbol(':'), Ident("char".into())]
                ),
                Symbol('-'),
                Symbol('>'),
                Ident("return".into()),
                Group(
                    Braces,
                    vec![
                        NewLine,
                        Ident("let".into()),
                        Ident("strings".into()),
                        Symbol('='),
                        Ident("vec".into()),
                        Symbol('!'),
                        Group(Brackets, vec![Value(String("A String".into()))]),
                        Symbol(';'),
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
                        Value(Int(2)),
                        Symbol('*'),
                        Value(Int(2)),
                        Symbol(';'),
                        NewLine,
                        Ident("while".into()),
                        Group(
                            Braces,
                            vec![
                                NewLine,
                                Ident("let".into()),
                                Ident("x".into()),
                                Symbol('='),
                                Value(Float(5.)),
                                Symbol('+'),
                                Value(Float(5.6)),
                                Symbol(';'),
                                NewLine
                            ]
                        ),
                        NewLine,
                        Ident("function_call".into()),
                        Group(Parens, vec![]),
                        Symbol(';'),
                        NewLine
                    ]
                ),
                NewLine,
                Value(String("String}(]".into())),
                Symbol('.'),
                Ident("into".into()),
                Group(Parens, vec![]),
                Group(Brackets, vec![Value(Int(0))]),
                Symbol(';'),
                NewLine,
                NewLine,
                Value(Int(10)),
                Symbol('-'),
                Value(Int(1)),
                Symbol(';'),
                NewLine,
                Value(Float(2.)),
                Symbol('/'),
                Value(Float(10.1)),
                Symbol(';'),
                NewLine
            ]
        );
        Ok(())
    }
}
