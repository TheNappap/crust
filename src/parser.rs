mod block_definitions;
mod syntax_tree;
mod parse_ops;


pub use crate::error::Result;
use itertools::Itertools;
pub use syntax_tree::{fn_expr::{Fn, Signature}, BinOpKind, UnOpKind, Expression, SyntaxTree, types::Type};

use crate::{
    error::Error,
    lexer::{blockify, Block, Token, Delimeter},
};

use self::{block_definitions::{BlockDefinitions, call, returns, fn_def, print, assign, binary_ops, unary_ops, bools, conditional, loops, array, iter}, parse_ops::TokenList};

pub fn parse(source: &str) -> Result<SyntaxTree> {
    Parser::new().parse_code(source)
}

fn block_definitions() -> BlockDefinitions {
    let mut blockdefs = BlockDefinitions::new();
    blockdefs.add::<call::Call>();
    blockdefs.add::<returns::Return>();
    blockdefs.add::<fn_def::FnDef>();
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
        let fns = blockify(source)
            .map(|block| -> Result<Fn> {
                let block = block?;
                let tag = block.tag.clone();
                match self.parse_block_expression(block)? {
                    Expression::Fn(fun) => Ok(fun),
                    _ => {
                        Err(Error::syntax(
                            format!("The block '{}' cannot be used in this position.", tag),
                            0,
                        ))
                    }
                }
            })
            .try_collect()?;
        Ok(SyntaxTree::new(fns))
    }

    pub fn parse_expression(&self, tokens: Vec<Token>) -> Result<Expression> {
        if tokens.len() == 1 {
            match tokens.first().unwrap() {
                Token::Ident(name) => return Ok(Expression::Symbol(name.clone(), Type::Inferred)),
                Token::Literal(literal) => return Ok(Expression::Literal(literal.clone())),
                Token::Group(Delimeter::Parens, tokens) => return self.parse_expression(tokens.clone()),
                Token::Group(Delimeter::Brackets, _) => return self.parse_block_expression(Block { tag: "array".into(), header: tokens, body: vec![], chain: None }),
                _ => todo!(),
            }
        }

        let block = parse_ops::parse_expression(tokens)?;
        self.parse_block_expression(block)
    }

    pub fn parse_list(&self, tokens: Vec<Token>) -> TokenList {
        parse_ops::parse_list(tokens)
    }

    fn parse_block_expression(&self, block: Block) -> Result<Expression> {
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
