use super::{Expression, Type};
use core::slice::{Iter, IterMut};

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    name: String,
    params: Vec<Type>,
    returns: Type,
}

impl Signature {
    pub fn new(name: &str, params: Vec<Type>, returns: Type) -> Signature {
        Signature { name: name.to_string(), params, returns }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> Iter<Type> {
        self.params.iter()
    }

    pub fn params_mut(&mut self) -> IterMut<Type> {
        self.params.iter_mut()
    }

    pub fn returns(&self) -> &Type {
        &self.returns
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    signature: Signature,
    params: Vec<String>,
    exprs: Vec<Expression>,
}

impl Fn {
    pub fn new(signature: Signature, params: Vec<String>, exprs: Vec<Expression>) -> Fn {
        assert_eq!(params.len(), signature.params().len());
        Fn {
            signature,
            params,
            exprs,
        }
    }

    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    pub fn params(&self) -> Iter<String> {
        self.params.iter()
    }

    pub fn expressions(&self) -> Iter<Expression> {
        self.exprs.iter()
    }

    pub fn expressions_mut(&mut self) -> IterMut<Expression> {
        self.exprs.iter_mut()
    }
}
