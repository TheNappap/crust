use std::{collections::HashMap, rc::Rc};

use crate::{
    error::{Result, ThrowablePosition, ErrorKind},
    lexer::{Token, Span},
};

use super::{syntax_tree::Expression, Parser, ExpressionKind};

pub mod dot;
pub mod call;
pub mod fn_def;
pub mod print;
pub mod assign;
pub mod binary_ops;
pub mod unary_ops;
pub mod returns;
pub mod bools;
pub mod conditional;
pub mod loops;
pub mod group;
pub mod array;
pub mod iter;
pub mod data;
pub mod pattern_match;

pub trait BlockDefinition {
    fn id(&self) -> &str;
    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind>;
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind>;
}

pub struct BlockDefinitions {
    definitions: HashMap<String, Rc<dyn BlockDefinition>>,
}

impl BlockDefinitions {
    pub fn new() -> BlockDefinitions {
        BlockDefinitions {
            definitions: HashMap::new(),
        }
    }

    pub fn get(&self, tag: &str, span: &Span) -> Result<Rc<dyn BlockDefinition>> {
        self.definitions
            .get(tag)
            .cloned()
            .ok_or(span.error(ErrorKind::Syntax, format!("Definition for block not found: {}", tag)))
    }

    pub fn add<T: BlockDefinition + Default + 'static>(&mut self) {
        let definition: Rc<dyn BlockDefinition> = Rc::new(T::default());
        let key = definition.id().to_string();
        if self.definitions.contains_key(&key) {
            println!("Definition of block \"{}\" was overidden.", key);
        }
        self.definitions
            .insert(definition.id().to_string(), definition);
    }
}
