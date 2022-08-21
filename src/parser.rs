mod block_definitions;
mod syntax_tree;

use std::rc::Rc;

pub use crate::error::Result;
pub use syntax_tree::{fn_expr::Fn, Type, Expression, SyntaxTree};

use crate::{
    error::Error,
    lexer::{blockify, Block, BlockStream, Token, Delimeter},
};

use self::block_definitions::BlockDefinitions;

pub fn parse(source: &str) -> Result<SyntaxTree> {
    Parser::new().parse_code(source)
}

fn block_definitions() -> BlockDefinitions {
    let mut blockdefs = BlockDefinitions::new();
    blockdefs.add(Rc::new(block_definitions::call::Call));
    blockdefs.add(Rc::new(block_definitions::fn_def::FnDef));
    blockdefs.add(Rc::new(block_definitions::print::Print));
    blockdefs.add(Rc::new(block_definitions::print::PrintLn));
    blockdefs.add(Rc::new(block_definitions::assign::Let));
    blockdefs.add(Rc::new(block_definitions::binary_ops::Add));
    blockdefs
}

pub struct Parser {
    blockdefs: BlockDefinitions,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            blockdefs: block_definitions(),
        }
    }

    pub fn parse_code(&self, source: &str) -> Result<SyntaxTree> {
        let fns = blockify(source)
            .map(|block| -> Result<Fn> {
                let block = block?;
                let tag = block.tag.clone();
                match self.parse_block(block)? {
                    Expression::Fn(fun) => Ok(fun),
                    _ => {
                        return Err(Error::syntax(
                            format!("The block '{}' cannot be used in this position.", tag),
                            0,
                        )
                        .into())
                    }
                }
            })
            .collect::<Result<_>>()?;
        Ok(SyntaxTree::new(fns))
    }

    pub fn parse_block(&self, block: Block) -> Result<Expression> {
        Ok(self.blockdefs.get(&block.tag)?.parse(block, self)?)
    }

    pub fn parse_expression(&self, tokens: Vec<Token>) -> Result<Expression> {
        if tokens.len() == 1 {
            match tokens.first().unwrap() {
                Token::Ident(name) => return Ok(Expression::Symbol(name.clone(), Type::Inferred)),
                Token::Literal(literal) => return Ok(Expression::Literal(literal.clone())),
                Token::Symbol(_) => todo!(),
                _ => todo!(),
            }
        }

        let mut blocks = BlockStream::new(tokens);
        let first = blocks.next();
        let second = blocks.next();
        if first.is_none() {
            return Err(Error::syntax("Expected an expression".to_string(), 0).into());
        } else if second.is_some() {
            return Err(Error::syntax("Unexpected block after expression".to_string(), 0).into());
        }

        self.parse_block(first.unwrap()?)
    }

    pub fn parse_list(&self, tokens: Vec<Token>) -> TokenList {
        let tokens = if tokens.len() == 1 {
            if let Some(Token::Group(Delimeter::Parens, tokens)) = tokens.first() {
                tokens.clone()
            } else { tokens }
        } else { tokens };

        TokenList::from(tokens)
    }
}

pub struct TokenList {
    contents: Vec<Vec<Token>>
}

impl TokenList {
    fn from(tokens: Vec<Token>) -> TokenList {
        let mut tokens = tokens.into_iter();
        let mut contents = Vec::new();
        while let Some(element) = Self::take_element(&mut tokens) {
            contents.push(element)
        }
        TokenList { contents }
    }

    fn take_element(tokens: &mut impl Iterator<Item=Token>) -> Option<Vec<Token>> {
        let mut element = Vec::new();
        loop {
            match tokens.next() {
                None | Some(Token::Symbol(',')) => break,
                Some(token) => element.push(token)
            }
        }
        if element.is_empty() { None } else { Some(element) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::Literal};

    #[test]
    fn parser_test() -> Result<()> {
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
        let syntax_tree = parse(s)?;

        let print_call = |string: String| {
            Expression::Call(
                "__stdio_common_vfprintf".to_string(),
                vec![
                    Expression::Literal(Literal::Int(0)), 
                    Expression::Call(
                        "__acrt_iob_func".to_string(), 
                        vec![Expression::Literal(Literal::Int(1))], 
                        vec![Type::Int]
                    ), 
                    Expression::Literal(Literal::String(string)), 
                    Expression::Literal(Literal::Int(0)), 
                    Expression::Literal(Literal::Int(0))
                ],
                vec![],
            )
        };
        assert_eq!(
            syntax_tree,
            SyntaxTree::new(vec![Fn::new(
                "main",
                vec![
                    Expression::Fn(Fn::new(
                        "function",
                        vec![
                            print_call("Line1\n".to_string()),
                            print_call("Line2\n".to_string()),
                            print_call("Line3\n".to_string()),
                        ]
                    )),
                    Expression::Fn(Fn::new(
                        "f2",
                        vec![
                            print_call("one liner\n".to_string()),
                        ]
                    )),
                    Expression::Call("f2".to_string(), vec![], vec![]),
                    Expression::Call("function".to_string(), vec![], vec![])
                ]
            )])
        );
        Ok(())
    }
}
