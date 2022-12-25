use crate::parser::Type;


#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Pattern {
    ty: String,
    name: String,
}

impl Pattern {
    pub fn new(ty: String, name: String) -> Self {
        Pattern{ ty, name }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn matches_on(&self, ty: &Type) -> bool {
        self.ty == ty.name()
    }
}