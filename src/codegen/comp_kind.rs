use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

pub enum CompKind {
    Equal,
    NotEqual,
    LessThan,
    LessEquals,
    GreatThan,
    GreatEquals,
}

impl CompKind {
    pub fn to_intcc(self) -> IntCC {
        match self {
            CompKind::Equal => IntCC::Equal,
            CompKind::NotEqual => IntCC::NotEqual,
            CompKind::LessThan => IntCC::SignedLessThan,
            CompKind::LessEquals => IntCC::SignedLessThanOrEqual,
            CompKind::GreatThan => IntCC::SignedGreaterThan,
            CompKind::GreatEquals => IntCC::SignedGreaterThanOrEqual,
        }
    }
    
    pub fn to_floatcc(self) -> FloatCC {
        match self {
            CompKind::Equal => FloatCC::Equal,
            CompKind::NotEqual => FloatCC::NotEqual,
            CompKind::LessThan => FloatCC::LessThan,
            CompKind::LessEquals => FloatCC::LessThanOrEqual,
            CompKind::GreatThan => FloatCC::GreaterThan,
            CompKind::GreatEquals => FloatCC::GreaterThanOrEqual,
        }
    }
}