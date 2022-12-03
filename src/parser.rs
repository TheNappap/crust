mod blocks;
mod block_definitions;
mod syntax_tree;
mod parse_ops;
mod parse_list;


pub use crate::error::Result;
use itertools::Itertools;
pub use syntax_tree::{fn_expr::{Fn, Signature}, data::Data, BinOpKind, UnOpKind, Expression, SyntaxTree, Library, types::Type};

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
    blockdefs.add::<data::Struct>();
    blockdefs.add::<data::Enum>();
    blockdefs.add::<data::New>();
    blockdefs.add::<member::Field>();
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
        let mut fns = Vec::new();
        let mut datas = Vec::new();
        BlockStream::from(source)
            .try_for_each(|block| {
                let block = match block {
                    Ok(block) => block,
                    Err(err) => return Err(err),
                };
                let tag = block.tag.clone();
                match self.parse_block_expression(block) {
                    Ok(Expression::Fn(fun)) => fns.push(fun),
                    Ok(Expression::Data(data)) => datas.push(data),
                    Err(err) => return Err(err),
                    _ => return Err(Error::syntax(format!("The block '{}' cannot be used in this position.", tag), 0,))
                }
                Ok(())
            })?;

        Ok(SyntaxTree::new(fns, datas))
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
    
    fn parse_param(&self, tokens: Vec<Token>) -> Result<(String, Vec<Token>)> {
        let mut tokens = tokens.into_iter();
        let name = match tokens.next() {
            Some(Token::Ident(name)) => name,
            _ => return Err(Error::syntax("Expected an identifier as parameter name".to_string(), 0))
        };

        if !matches!(tokens.next(), Some(Token::Operator(Operator::Colon))) {  
            return Err(Error::syntax("Expected an ':'".to_string(), 0))
        }
        Ok((name, tokens.collect()))
    }

    fn parse_parameter(&self, tokens: Vec<Token>) -> Result<(String, Type)> {
        self.parse_param(tokens).and_then(|(s,t)| Ok((s, Type::from(t[0].clone())?)))
    }
    
    fn parse_param_expression(&self, tokens: Vec<Token>) -> Result<(String, Expression)> {
        self.parse_param(tokens).and_then(|(s,t)| Ok((s, self.parse_expression(t)?)))
    }

    fn parse_block_expression(&self, block: Block) -> Result<Expression> {
        if block.is_anonymous() {
            if block.header.len() > 1 {
                return Err(Error::syntax("Can't parse anonymous block.".into(), 0));
            }

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
            )
        ], 
        vec![]));
        Ok(())
    }
}
