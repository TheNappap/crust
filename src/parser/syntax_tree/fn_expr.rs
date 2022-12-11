use super::{Expression, types::Type};
use core::slice::{Iter, IterMut};

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

    pub fn set_self_type(&mut self, ty: Type) {
        self.signature.self_ty = Some(ty.clone());
        if let Some((name, self_ty)) = self.params_mut().next() {
            if name == "self" {
                *self_ty = ty;
            }
        }
    }

    pub fn params(&self) -> impl Iterator<Item=(&String,&Type)> {
        self.params.iter().zip(self.signature.params())
    }

    pub fn params_mut(&mut self) -> impl Iterator<Item=(&String,&mut Type)> {
        self.params.iter().zip(self.signature.params_mut())
    }

    pub fn body(&self) -> Iter<Expression> {
        self.body.iter()
    }

    pub fn body_mut(&mut self) -> IterMut<Expression> {
        self.body.iter_mut()
    }
}
