mod blocks;
mod block_definitions;
mod syntax_tree;
mod parse_ops;
mod parse_list;


pub use crate::error::Result;
use itertools::Itertools;
pub use syntax_tree::{fn_expr::{Fn, Signature}, BinOpKind, UnOpKind, Expression, SyntaxTree, types::Type};

use crate::{
    error::Error,
    lexer::{Token, Delimeter, Operator},
};

use self::{block_definitions::*, blocks::{BlockStream, Block}};

pub fn parse(source: &str) -> Result<SyntaxTree> {
    Parser::new().parse_code(source)
}

fn block_definitions() -> BlockDefinitions {
    let mut blockdefs = BlockDefinitions::new();
    blockdefs.add::<call::Call>();
    blockdefs.add::<returns::Return>();
    blockdefs.add::<fn_def::FnDef>();
    blockdefs.add::<structs::Struct>();
    blockdefs.add::<print::Print>();
    blockdefs.add::<print::PrintLn>();
    blockdefs.add::<assign::Let>();
    blockdefs.add::<assign::Mut>();
    blockdefs.add::<binary_ops::Add>();
    blockdefs.add::<binary_ops::Subtract>();
    blockdefs.add::<binary_ops::Multiply>();
    blockdefs.add::<binary_ops::Divide>();
    blockdefs.add::<binary_ops::Equals>();
    blockdefs.add::<binary_ops::NotEquals>();
    blockdefs.add::<unary_ops::Negate>();
    blockdefs.add::<bools::True>();
    blockdefs.add::<bools::False>();
    blockdefs.add::<conditional::If>();
    blockdefs.add::<conditional::Else>();
    blockdefs.add::<loops::While>();
    blockdefs.add::<loops::For>();
    blockdefs.add::<group::Group>();
    blockdefs.add::<array::Array>();
    blockdefs.add::<array::Index>();
    blockdefs.add::<iter::Iter>();
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
        let fns = BlockStream::from(source)
            .filter_map(|block| {
                let block = match block {
                    Ok(block) => block,
                    Err(err) => return Some(Err(err)),
                };
                let tag = block.tag.clone();
                match self.parse_block_expression(block) {
                    Ok(Expression::Fn(fun)) => Some(Ok(fun)),
                    Ok(Expression::Struct(_, _)) => None,
                    Err(err) => Some(Err(err)),
                    _ => {
                        Some(Err(Error::syntax(
                            format!("The block '{}' cannot be used in this position.", tag),
                            0,
                        )))
                    }
                }
            })
            .try_collect()?;
        Ok(SyntaxTree::new(fns))
    }

    fn parse_expression(&self, mut tokens: Vec<Token>) -> Result<Expression> {
        if tokens.is_empty() {
            return Err(Error::syntax("Can't make block from an empty list of tokens.".into(), 0));
        }

        if tokens.len() == 1 {
            let block = Block::anonymous_block(tokens);
            return self.parse_block_expression(block);
        }

        parse_ops::parse_operators(&mut tokens);
        match BlockStream::new(tokens).next().transpose()? {
            Some(block) => self.parse_block_expression(block),
            None => return Err(Error::syntax("Can't make block from an empty list of tokens.".into(), 0)),
        }
    }

    fn parse_list(&self, tokens: Vec<Token>) -> Vec<Vec<Token>> {
        parse_list::parse_list(tokens)
    }

    fn parse_group(&self, tokens: Vec<Token>) -> Result<Vec<Expression>> {
        BlockStream::new(tokens)
            .map(|b|self.parse_block_expression(b?))
            .try_collect()
    }
    
    fn parse_parameter(&self, tokens: Vec<Token>) -> Result<(String, Type)> {
        let mut tokens = tokens.into_iter();
        let name = match tokens.next() {
            Some(Token::Ident(name)) => name,
            _ => return Err(Error::syntax("Expected an identifier as parameter name".to_string(), 0))
        };

        if !matches!(tokens.next(), Some(Token::Operator(Operator::Colon))) {  
            return Err(Error::syntax("Expected an ':' and a type name".to_string(), 0))
        }
        let ty = match tokens.next() {
            Some(token) => Type::from(token),
            _ => return Err(Error::syntax("Expected an identifier as type name".to_string(), 0))
        }; 
        Ok((name, ty))
    }

    fn parse_block_expression(&self, block: Block) -> Result<Expression> {
        if block.is_anonymous() && block.header.len() == 1 {
            assert!(block.body.is_empty());
            let block = match block.header.first().unwrap() {
                Token::Ident(name) => return Ok(Expression::Symbol(name.clone(), Type::Inferred)),
                Token::Literal(literal) => return Ok(Expression::Literal(literal.clone())),
                Token::Group(Delimeter::Parens, tokens) => return self.parse_expression(tokens.clone()),
                Token::Group(Delimeter::Brackets, _) => Block { tag: "array".into(), header: block.header, body: vec![], chain: None },
                Token::Group(Delimeter::Braces, body) => Block { tag: "group".into(), header: vec![], body: body.clone(), chain: None },
                _ => return Err(Error::syntax("Can't parse anonymous block.".into(), 0)),
            };
            return self.parse_block_expression(block);
        }
        if block.is_anonymous() {
            return Err(Error::syntax("Can't parse anonymous block.".into(), 0));
        }

        let expr = self.blockdefs.get(&block.tag)?.parse(block.header, block.body, self);
        match block.chain {
            Some(chain) => self.parse_chained_block_expression(*chain, expr?),
            None => expr,
        } 
    }
    
    fn parse_chained_block_expression(&self, block: Block, input: Expression) -> Result<Expression> {
        let expr = self.blockdefs.get(&block.tag)?.parse_chained(block.header, block.body, input, self);
        match block.chain {
            Some(chain) => self.parse_chained_block_expression(*chain, expr?),
            None => expr,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::Literal, parser::syntax_tree::fn_expr::Signature};

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
                Signature::new("__stdio_common_vfprintf",vec![Type::Int,Type::Int,Type::String,Type::Int,Type::Int],Type::Void),
                vec![
                    Expression::Literal(Literal::Int(0)), 
                    Expression::Call(Signature::new("__acrt_iob_func", vec![Type::Int], Type::Int), vec![Expression::Literal(Literal::Int(1))]), 
                    Expression::Literal(Literal::String(string)), 
                    Expression::Literal(Literal::Int(0)), 
                    Expression::Literal(Literal::Int(0))
                ],
            )
        };
        assert_eq!(
            syntax_tree,
            SyntaxTree::new(vec![Fn::new(
                Signature::new("main",vec![], Type::Void),
                vec![],
                vec![
                    Expression::Fn(Fn::new(
                        Signature::new("function", vec![], Type::Void),
                        vec![],
                        vec![
                            print_call("Line1".to_string()),
                            print_call("Line2".to_string()),
                            print_call("Line3".to_string()),
                        ],
                    )),
                    Expression::Fn(Fn::new(
                        Signature::new("f2", vec![], Type::Void),
                        vec![],
                        vec![
                            print_call("one liner".to_string()),
                        ],
                    )),
                    Expression::Call(Signature::new("f2", vec![], Type::Inferred), vec![]),
                    Expression::Call(Signature::new("function", vec![], Type::Inferred), vec![]),
                ],
            )])
        );
        Ok(())
    }
}
