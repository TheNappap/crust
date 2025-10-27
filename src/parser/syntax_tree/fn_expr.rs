use super::{Expression, types::Type};
use core::{slice::{Iter, IterMut}};

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    self_ty: Option<Type>,
    name: String,
    params: Vec<Type>,
    returns: Type,
}

impl Signature {
    pub fn new(self_ty: Option<Type>, name: &str, params: Vec<Type>, returns: Type) -> Signature {
        Signature { self_ty, name: name.to_string(), params, returns }
    }
    
    pub fn self_ty_mut(&mut self) -> &mut Option<Type> {
        &mut self.self_ty
    }

    pub fn self_ty(&self) -> &Option<Type> {
        &self.self_ty
    }

    pub fn set_self_type(&mut self, type_name: &str) {
        self.name = type_name.to_owned() + "::" + &self.name;
        self.self_ty = Some(Type::Named(type_name.to_owned()));
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> Iter<'_, Type> {
        self.params.iter()
    }

    pub fn params_mut(&mut self) -> IterMut<'_, Type> {
        self.params.iter_mut()
    }

    pub fn returns(&self) -> &Type {
        &self.returns
    }

    pub fn returns_mut(&mut self) -> &mut Type {
        &mut self.returns
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fn {
    signature: Signature,
    params: Vec<String>,
    body: Vec<Expression>,
}

impl Fn {
    pub fn new(signature: Signature, params: Vec<String>, body: Vec<Expression>) -> Fn {
        assert_eq!(params.len(), signature.params().len());
        Fn {
            signature,
            params,
            body,
        }
    }

    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    pub fn signature_mut(&mut self) -> &mut Signature {
        &mut self.signature
    }

    pub fn set_self_type(&mut self, type_name: &str) {
        self.signature.set_self_type(type_name);
        if let Some(name) = self.params.first() {
            if name == "self" {
                let ty = Type::Named(type_name.to_owned());
                self.signature.self_ty = Some(ty.clone());
                self.signature.params[0] = ty;
            }
        }
    }

    pub fn params(&self) -> impl Iterator<Item=(&String,&Type)> {
        self.params.iter().zip(self.signature.params())
    }

    pub fn params_mut(&mut self) -> impl Iterator<Item=(&String,&mut Type)> {
        self.params.iter().zip(self.signature.params_mut())
    }

    pub fn body(&self) -> Iter<'_, Expression> {
        self.body.iter()
    }

    pub fn body_mut(&mut self) -> IterMut<'_, Expression> {
        self.body.iter_mut()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Trait {
    pub name: String,
    pub sigs: Vec<Signature>,
    pub fns: Vec<Fn>,
}

impl Trait {
    pub fn new(name: String, sigs: Vec<Signature>, fns: Vec<Fn>) -> Self {
        Trait {
            name,
            sigs,
            fns,
        }
    }
}