use crate::parser::Type;


#[derive(Debug, PartialEq, Clone)]
pub struct Data {
    name: String,
    ty: Type,
}

impl Data {
    pub fn new(name: String, ty: Type,) -> Self {
        Data { name, ty }
    }

    pub fn name(&self) -> &String {
        &self.name
    }
    
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}