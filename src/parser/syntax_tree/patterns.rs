use crate::parser::Type;


#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Pattern {
    Ident(String),
    EnumVariant(String, String),
}

impl Pattern {
    pub fn matches_on(&self, ty: &Type) -> bool {
        match self {
            Pattern::Ident(_) => true,
            Pattern::EnumVariant(ty_name, _) => *ty_name == ty.name(),
        }
    }
}