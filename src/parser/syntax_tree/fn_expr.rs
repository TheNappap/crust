use super::Expression;
use core::slice::Iter;

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    name: String,
    exprs: Vec<Expression>,
}

impl Fn {
    pub fn new(name: &str, exprs: Vec<Expression>) -> Fn {
        Fn {
            name: name.to_string(),
            exprs,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn expressions(&self) -> Iter<Expression> {
        self.exprs.iter()
    }
}
