use std::{collections::HashMap, rc::Rc};

use crate::{
    error::{Error, Result},
    lexer::Block,
};

use super::{syntax_tree::Expression, Parser};

pub mod call;
pub mod fn_def;
pub mod print;
pub mod assign;
pub mod binary_ops;
pub mod returns;
pub mod bool;

pub trait BlockDefinition {
    fn id(&self) -> &str;
    fn parse(&self, block: Block, parser: &Parser) -> Result<Expression>;
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

    pub fn get(&self, tag: &str) -> Result<Rc<dyn BlockDefinition>> {
        self.definitions
            .get(tag)
            .cloned()
            .ok_or(Error::syntax(format!("Definition for block not found: {}", tag), 0).into())
    }

    pub fn add(&mut self, definition: Rc<dyn BlockDefinition>) {
        let key = definition.id().to_string();
        if self.definitions.contains_key(&key) {
            println!("Definition of block \"{}\" was overidden.", key);
        }
        self.definitions
            .insert(definition.id().to_string(), definition);
    }
}
