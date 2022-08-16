mod block_definitions;
mod syntax_tree;

use std::rc::Rc;

pub use crate::error::Result;
pub use syntax_tree::{fn_expr::Fn, Type, Expression, Literal, SyntaxTree};

use crate::{
    error::Error,
    lexer::{blockify, Block, BlockStream, Token},
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
    blockdefs.add(Rc::new(block_definitions::let_def::Let));
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

    pub fn _parse_tokens(&self, tokens: Vec<Token>) -> BlockStream {
        BlockStream::new(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Result;

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
        assert_eq!(
            syntax_tree,
            SyntaxTree::new(vec![Fn::new(
                "main",
                vec![
                    Expression::Fn(Fn::new(
                        "function",
                        vec![
                            Expression::Call(
                                "puts".to_string(),
                                vec![Expression::Literal(Literal::String("Line1".to_string()))],
                                vec![],
                            ),
                            Expression::Call(
                                "puts".to_string(),
                                vec![Expression::Literal(Literal::String("Line2".to_string()))],
                                vec![],
                            ),
                            Expression::Call(
                                "puts".to_string(),
                                vec![Expression::Literal(Literal::String("Line3".to_string()))],
                                vec![],
                            )
                        ]
                    )),
                    Expression::Fn(Fn::new(
                        "f2",
                        vec![Expression::Call(
                            "puts".to_string(),
                            vec![Expression::Literal(Literal::String(
                                "one liner".to_string()
                            ))],
                            vec![],
                        )]
                    )),
                    Expression::Call("f2".to_string(), vec![], vec![]),
                    Expression::Call("function".to_string(), vec![], vec![])
                ]
            )])
        );
        Ok(())
    }
}
