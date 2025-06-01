mod blocks;
mod block_definitions;
mod syntax_tree;
mod parse_ops;
mod parse_list;


pub use crate::error::Result;
use itertools::Itertools;
pub use syntax_tree::{fn_expr::{Fn, Signature}, BinOpKind, UnOpKind, Expression, ExpressionKind, patterns::Pattern, SyntaxTree, Library, types::Type, ordered_map::OrderedMap};

use crate::{
    error::{ThrowablePosition},
    lexer::{Token, Delimeter, Operator, TokenKind},
};

use self::{block_definitions::*, blocks::{BlockStream, Block}};

pub fn parse(source: &str) -> Result<SyntaxTree> {
    Parser::new().parse_code(source)
}

fn block_definitions() -> BlockDefinitions {
    let mut blockdefs = BlockDefinitions::new();
    blockdefs.add::<dot::Dot>();
    blockdefs.add::<call::Call>();
    blockdefs.add::<returns::Return>();
    blockdefs.add::<fn_def::FnDef>();
    blockdefs.add::<fn_def::Impl>();
    blockdefs.add::<traits::Trait>();
    blockdefs.add::<data::Struct>();
    blockdefs.add::<data::Enum>();
    blockdefs.add::<data::New>();
    blockdefs.add::<data::Field>();
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
    blockdefs.add::<pattern_match::Match>();
    blockdefs.add::<pattern_match::Case>();
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
        let mut traits = Vec::new();
        let mut impls = Vec::new();

        BlockStream::from(source)
            .try_for_each(|block| {
                let block = match block {
                    Ok(block) => block,
                    Err(err) => return Err(err),
                };
                let tag = block.tag.clone();
                let parsed_expression = self.parse_block_expression(block.clone())?;
                match parsed_expression.kind {
                    ExpressionKind::Fn(fun) => fns.push(fun.clone()),
                    ExpressionKind::Data(data) => datas.push((data.clone(), parsed_expression.span.to_owned())),
                    ExpressionKind::Impl(type_name, methods, trait_name) => impls.push((type_name, methods, trait_name, parsed_expression.span)),
                    ExpressionKind::Trait(trait_) => traits.push(trait_.clone()),
                    _ => return parsed_expression.span.syntax(format!("The block '{}' cannot be used in this position.", tag))
                }
                Ok(())
            })?;
        
        for (type_name, methods, trait_name, span) in impls {
            if let Some(trait_name) = trait_name {
                if let Ok(i) = traits.binary_search_by_key(&trait_name, |t| t.name.clone()) {
                    let t = &traits[i];
                    t.fns.iter().cloned().for_each(|mut fun| {
                        fun.set_self_type(&type_name);
                        fns.push(fun)
                    });
                    let _: () = t.sigs.iter().cloned().map(|mut sig| {
                        sig.set_self_type(&type_name);
                        let has_impl = methods.iter().any(|fun| fun.signature().name() == sig.name());
                        if !has_impl {
                            return span.syntax("Not all trait functions implemented".to_string());
                        }
                        Ok(())
                    }).try_collect()?;
                    let _: () = methods.into_iter().map(|fun| {
                        let is_in_trait = t.sigs.iter().cloned().any(|mut sig| {
                            sig.set_self_type(&type_name);
                            fun.signature().name() == sig.name()
                        });
                        if !is_in_trait {
                            return span.syntax("Function not part of trait".to_string());
                        }
                        fns.push(fun.clone());
                        Ok(())
                    }).try_collect()?;
                }
            } else {
                methods.into_iter().for_each(|fun| fns.push(fun.clone()));
            }
        }

        Ok(SyntaxTree::new(fns, vec![], datas, traits))
    }

    fn parse_expression(&self, mut tokens: Vec<Token>) -> Result<Expression> {
        if tokens.is_empty() {
            panic!("Can't make block from an empty list of tokens.");
        }

        if tokens.len() == 1 {
            let block = Block::anonymous_block(tokens);
            return self.parse_block_expression(block);
        }

        parse_ops::parse_operators(&mut tokens);
        let span = tokens[0].span.clone();
        match BlockStream::new(tokens).next().transpose()? {
            Some(block) => self.parse_block_expression(block),
            None => return span.syntax("Can't make block from these tokens.".into()),
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
        assert!(tokens.len() > 0);
        let mut tokens = tokens.into_iter();
        let name = match tokens.next() {
            Some(Token{kind: TokenKind::Ident(name), ..}) => name,
            Some(token) => return token.span.syntax("Expected an identifier as parameter name".to_string()),
            None => unreachable!(),
        };

        match tokens.next() {  
            Some(Token{kind: TokenKind::Operator(Operator::Colon), span: _}) => Ok((name, tokens.collect())),
            Some(Token{kind: _, span}) => return span.syntax("Expected an ':'".to_string()),
            _ => Ok((name, tokens.collect())),
        }
    }

    fn parse_parameter(&self, tokens: Vec<Token>) -> Result<(String, Type)> {
        assert!(tokens.len() > 0);
        match &tokens[0].kind {
            TokenKind::Ident(name) if name == "self" => {
                assert!(tokens.len() == 1);
                return Ok((name.to_owned(), Type::Inferred));
            }
            _ => ()
        }
        self.parse_param(tokens).and_then(|(s,t)| Ok((s, Type::from(t[0].clone())?)))
    }
    
    fn parse_param_expression(&self, tokens: Vec<Token>) -> Result<(String, Expression)> {
        self.parse_param(tokens).and_then(|(s,t)| Ok((s, self.parse_expression(t)?)))
    }

    fn parse_block_expression(&self, block: Block) -> Result<Expression> {
        if block.is_anonymous() {
            if block.header.len() > 1 {
                return block.span.syntax("Can't parse anonymous block.".into());
            }

            assert!(block.body.is_empty());
            let token = block.header.first().unwrap();
            let block = match &token.kind {
                TokenKind::Ident(name) => return Ok(Expression::new(ExpressionKind::Symbol(name.clone(), Type::Inferred), token.span.clone())),
                TokenKind::Literal(literal) => return Ok(Expression::new(ExpressionKind::Literal(literal.clone()), token.span.clone())),
                TokenKind::Group(Delimeter::Parens, tokens) => return self.parse_expression(tokens.clone()),
                TokenKind::Group(Delimeter::Brackets, _) => Block { tag: "array".into(), span: block.span, header: block.header, body: vec![], chain: None },
                TokenKind::Group(Delimeter::Braces, body) => Block { tag: "group".into(), span: block.span, header: vec![], body: body.clone(), chain: None },
                _ => return token.span.syntax("Can't parse anonymous block.".into()),
            };
            return self.parse_block_expression(block);
        }

        let expr = self.blockdefs.get(&block.tag, &block.span)?
            .parse(&block.span, block.header, block.body, self)
            .map(|kind| Expression::new(kind, block.span));
        
        match block.chain {
            Some(chain) => self.parse_chained_block_expression(*chain, expr?),
            None => expr,
        } 
    }
    
    fn parse_chained_block_expression(&self, block: Block, input: Expression) -> Result<Expression> {
        let expr = self.blockdefs.get(&block.tag, &block.span)?
            .parse_chained(&block.span, block.header, block.body, input, self)
            .map(|kind| Expression::new(kind, block.span));
        match block.chain {
            Some(chain) => self.parse_chained_block_expression(*chain, expr?),
            None => expr,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{error::Result, lexer::{Literal, Span, Position}, parser::syntax_tree::fn_expr::Signature};

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

        let print_call = |string: String, span: Span| {
            Expression::new(ExpressionKind::Call(
                Signature::new(None, "__stdio_common_vfprintf",vec![Type::Int,Type::Int,Type::String,Type::Int,Type::Int],Type::Void),
                vec![
                    Expression::new(ExpressionKind::Literal(Literal::Int(0)), span.clone()), 
                    Expression::new(ExpressionKind::Call(Signature::new(None, "__acrt_iob_func", vec![Type::Int], Type::Int), 
                        vec![Expression::new(ExpressionKind::Literal(Literal::Int(1)), span.clone())])
                    , span.clone()), 
                    Expression::new(ExpressionKind::Literal(Literal::String(string)), span.clone()), 
                    Expression::new(ExpressionKind::Literal(Literal::Int(0)), span.clone()), 
                    Expression::new(ExpressionKind::Literal(Literal::Int(0)), span.clone())
                ],
            ), span.clone())
        };

        assert_eq!(
            syntax_tree,
            SyntaxTree::new(vec![Fn::new(
                Signature::new(None, "main", vec![], Type::Void),
                vec![],
                vec![
                    Expression::new(ExpressionKind::Fn(Fn::new(
                        Signature::new(None, "function", vec![], Type::Void),
                        vec![],
                        vec![
                            print_call("Line1".to_string(), Span::new(Position::new(3, 11), Position::new(3, 18))),
                            print_call("Line2".to_string(), Span::new(Position::new(4, 11), Position::new(4, 18))),
                            print_call("Line3".to_string(), Span::new(Position::new(5, 11), Position::new(5, 18))),
                        ],
                    )), Span::new(Position::new(2, 7), Position::new(6, 0))),
                    Expression::new(ExpressionKind::Fn(Fn::new(
                        Signature::new(None, "f2", vec![], Type::Void),
                        vec![],
                        vec![
                            print_call("one liner".to_string(), Span::new(Position::new(7, 19), Position::new(7, 30))),
                        ],
                    )), Span::new(Position::new(7, 7), Position::new(7, 30))),
                    Expression::new(ExpressionKind::Call(Signature::new(None, "f2", vec![], Type::Inferred), vec![]), Span::new(Position::new(9, 9), Position::new(9, 13))),
                    Expression::new(ExpressionKind::Call(Signature::new(None, "function", vec![], Type::Inferred), vec![]), Span::new(Position::new(10, 9), Position::new(10, 19))),
                ],
            )
        ], 
        vec![],
        vec![],
        vec![],));
        Ok(())
    }
}
