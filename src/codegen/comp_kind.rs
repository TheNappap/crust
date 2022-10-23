use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

pub enum CompKind {
    Equal,
    NotEqual
}

impl CompKind {
    pub fn to_intcc(self) -> IntCC {
        match self {
            CompKind::Equal => IntCC::Equal,
            CompKind::NotEqual => IntCC::NotEqual,
        }
    }
    
    pub fn to_floatcc(self) -> FloatCC {
        match self {
            CompKind::Equal => FloatCC::Equal,
            CompKind::NotEqual => FloatCC::NotEqual,
        }
    }
}