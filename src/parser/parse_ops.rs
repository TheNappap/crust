
use core::fmt;

use crate::{lexer::{Delimeter, Span, Token, TokenKind}, parser::{Expression, Parser, blocks::{Block, BlockTag}}, utils::Result};

impl TokenKind {
    pub fn is_operator(&self) -> bool {
        OperatorKind::from(self).is_some()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum OperatorKind {
    Dot,
    Not,
    Star, Slash,
    Plus, Dash,
    EqEq, Neq, 
    Less, LessEq,
    Great, GreatEq,
    Range,   
}

impl OperatorKind {
    pub fn from(token: &TokenKind) -> Option<Self> {
        match token {
            TokenKind::Dot => Some(OperatorKind::Dot),
            TokenKind::Plus => Some(OperatorKind::Plus),
            TokenKind::Dash => Some(OperatorKind::Dash),
            TokenKind::Star => Some(OperatorKind::Star),
            TokenKind::Slash => Some(OperatorKind::Slash),
            TokenKind::EqEq => Some(OperatorKind::EqEq),
            TokenKind::Neq => Some(OperatorKind::Neq),
            TokenKind::Not => Some(OperatorKind::Not),
            TokenKind::Less => Some(OperatorKind::Less),
            TokenKind::LessEq => Some(OperatorKind::LessEq),
            TokenKind::Great => Some(OperatorKind::Great),
            TokenKind::GreatEq => Some(OperatorKind::GreatEq),
            TokenKind::Range => Some(OperatorKind::Range),
            _ => None,
        }
    }    
    
    fn as_token(&self) -> TokenKind {
        match self {
            OperatorKind::Dot => TokenKind::Dot,
            OperatorKind::Plus => TokenKind::Plus,
            OperatorKind::Dash => TokenKind::Dash,
            OperatorKind::Star => TokenKind::Star,
            OperatorKind::Slash => TokenKind::Slash,
            OperatorKind::EqEq => TokenKind::EqEq,
            OperatorKind::Neq => TokenKind::Neq,
            OperatorKind::Not => TokenKind::Not,
            OperatorKind::Less => TokenKind::Less,
            OperatorKind::LessEq => TokenKind::LessEq,
            OperatorKind::Great => TokenKind::Great,
            OperatorKind::GreatEq => TokenKind::GreatEq,
            OperatorKind::Range => TokenKind::Range,
        }
    }
}

impl fmt::Display for OperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use OperatorKind::*;
        match self {
            Not => write!(f, "!"),
            Dash => write!(f, "-"),
            Dot => write!(f, "."),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Plus => write!(f, "+"),
            EqEq => write!(f, "=="),
            Neq => write!(f, "!="),
            Less => write!(f, "<"),
            LessEq => write!(f, "<="),
            Great => write!(f, ">"),
            GreatEq => write!(f, ">="),
            Range => write!(f, ".."),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum OperatorPosition {
    Prefix,
    _Postfix,
    Binary
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Operator {
    kind: OperatorKind,
    position: OperatorPosition,
}

impl Operator {
    fn new(kind: OperatorKind, position: OperatorPosition) -> Self {
        Self { kind, position }
    }

    fn precedence(&self) -> u8 {
        use OperatorKind::*;
        use OperatorPosition::*;
        match (&self.position, &self.kind) {
            (_Postfix, _) => todo!(),
            (_, Dot) => 1,
            (Prefix, _) => 2,
            (_, Star | Slash) => 3,
            (_, Plus | Dash) => 4,
            (_, EqEq | Neq | Less | LessEq | Great | GreatEq) => 5,
            (_, Range) => 6,
            (Binary, Not) => unimplemented!(),
        }
    }
}

// TODO implement indexing for general scenario's
pub fn parse_indexing(tokens: &mut Vec<Token>) {
    if tokens.len() < 2 || tokens.iter().any(|token| token.kind.is_operator()) {
        return;
    }
    let last_token = tokens.last().unwrap();
    if let TokenKind::Group(Delimeter::Brackets, _) = last_token.kind {
        let new_token = Token::new(TokenKind::Ident("index".into()), last_token.span.clone());
        tokens.insert(0, new_token);
    }
}

pub fn parse_operators(block: Block, parser: &Parser) -> Result<Expression> {
    if block.is_id_tagged() || block.chain.is_none() {
        return parser.parse_block_expression(block);
    }

    let ops_tree = OpsTree::new(block.clone());

    ops_tree.parse(parser)
}

// TODO refactor OpsTree
#[derive(Debug, PartialEq, Clone)]
enum OpsTree {
    Leaf(Vec<Token>),
    UnOp(OperatorKind, Span, Box<OpsTree>),
    BinOp(OperatorKind, Span, Box<OpsTree>, Box<OpsTree>),
}

impl OpsTree {
    fn new(block: Block) -> Self {
        let blocks: Vec<_> = OpsTree::get_chained_block_vec(block);
        let ops_list = OpsTree::to_operators(blocks);
        OpsTree::from_chained_operators(ops_list)
    }

    fn from_chained_operators(mut blocks: Vec<(Block, Option<Operator>)>) -> Self {
        assert!(!blocks.is_empty());
        let (i, op) = OpsTree::next_operator_index(&blocks);
        match op {
            None => {
                assert!(blocks[i].0.is_anonymous());
                OpsTree::Leaf(blocks[i].0.header.clone())
            }
            Some(operator) => {
                match operator.position {
                    OperatorPosition::Prefix => {
                        let span = blocks[i].0.span.clone();
                        blocks[i].0.tag = BlockTag::Anonymous;
                        blocks[i].1 = None;
                        OpsTree::UnOp(operator.kind, span, Box::new(OpsTree::from_chained_operators(blocks)))
                    },
                    OperatorPosition::Binary => {
                        assert!(i > 0);
                        let span = blocks[i].0.span.clone();
                        blocks[i].0.tag = BlockTag::Anonymous;
                        blocks[i].1 = None;
                        let end_list = blocks.split_off(i);
                        let left = Box::new(OpsTree::from_chained_operators(blocks));
                        let right = Box::new(OpsTree::from_chained_operators(end_list));
                        OpsTree::BinOp(operator.kind, span, left, right)
                    },
                    OperatorPosition::_Postfix => unreachable!(),
                }
            },
        }
    }

    fn next_operator_index(blocks: &Vec<(Block, Option<Operator>)>) -> (usize, Option<Operator>) {
        let mut current = (0, None);
        for (i, (_, op)) in blocks.iter().enumerate() {
            match current.1 {
                None => {
                    current = (i, op.clone())
                }
                Some(ref cur_op) => {
                    if let Some(op) = op && cur_op.precedence() < op.precedence() {
                        current = (i, Some(op.clone()))
                    }
                }
            }
        }
        current
    }

    fn parse(self, parser: &Parser) -> Result<Expression> {
        match self {
            OpsTree::Leaf(tokens) => parser.parse_expression(tokens),
            OpsTree::UnOp(kind, span, ops_tree) => {
                let tag = BlockTag::Operator(kind);
                let header = ops_tree.as_tokens();
                let block = Block { tag, span: span.clone(), header: vec![Token::new(TokenKind::Group(Delimeter::Parens, header), span.clone())], body: vec![], chain: None };
                parser.parse_block_expression(block)
            }
            OpsTree::BinOp(kind, span, ops_tree, ops_tree1) => {
                let tag = BlockTag::Operator(kind);
                let mut header = ops_tree.as_tokens();
                header.push(Token::new(TokenKind::Comma, span.clone()));
                header.extend(ops_tree1.as_tokens());
                let block = Block { tag, span: span.clone(), header: vec![Token::new(TokenKind::Group(Delimeter::Parens, header), span.clone())], body: vec![], chain: None };
                parser.parse_block_expression(block)
            }
        }
    }

    fn as_tokens(self) -> Vec<Token> {
        match self {
            OpsTree::Leaf(tokens) => tokens,
            OpsTree::UnOp(kind, span, ops_tree) => {
                let mut tokens = ops_tree.as_tokens();
                tokens.insert(0, Token::new(kind.as_token(), span.clone()));
                vec![Token::new(TokenKind::Group(Delimeter::Parens, tokens), span.clone())]
            }
            OpsTree::BinOp(kind, span, ops_tree, ops_tree1) => {
                let mut tokens = ops_tree.as_tokens();
                tokens.insert(0, Token::new(kind.as_token(), span.clone()));
                tokens.push(Token::new(TokenKind::Comma, span.clone()));
                tokens.extend(ops_tree1.as_tokens());
                vec![Token::new(TokenKind::Group(Delimeter::Parens, tokens), span.clone())]
            }
        }
    }

    fn get_chained_block_vec(mut block: Block) -> Vec<Block> {
        assert!(block.is_anonymous() || block.is_operator());
        match block.chain.take() {
            None => vec![block],
            Some(chain) => {
                let mut blocks = OpsTree::get_chained_block_vec(*chain);
                blocks.insert(0,block);
                blocks
            }
        }
    }

    fn to_operators(blocks: Vec<Block>) -> Vec<(Block, Option<Operator>)> {
        blocks.into_iter()
                    .scan(true, |is_last_header_empty, block| {
                        let header_is_empty = block.header.is_empty();
                        let value = match block.tag.clone() {
                            BlockTag::Anonymous => (block, None),
                            BlockTag::Operator(kind) => {
                                let operator = if *is_last_header_empty {
                                    Operator::new(kind, OperatorPosition::Prefix)
                                } else {
                                    Operator::new(kind, OperatorPosition::Binary)
                                };
                                (block, Some(operator))
                            },
                            BlockTag::Ident(_) => unreachable!(),
                        };
                        *is_last_header_empty = header_is_empty;
                        Some(value)
                    })
                    .collect()
    }
}