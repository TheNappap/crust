

pub mod fn_expr;
pub mod types;
pub mod expression;
pub mod field_map;

pub use expression::{Expression, BinOpKind, UnOpKind};

use self::fn_expr::Fn;

use super::{Type, Signature};

pub trait Library {
    fn fns(&self) -> Vec<Fn>;
    fn imported_fns(&self) -> Vec<Signature>;
    fn data_types(&self) -> Vec<Type>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct SyntaxTree {
    fns: Vec<Fn>,
    imports: Vec<Signature>,
    data_types: Vec<Type>,
}

impl SyntaxTree {
    pub fn new(fns: Vec<Fn>, imports: Vec<Signature>, data_types: Vec<Type>) -> SyntaxTree {
        SyntaxTree { fns, imports, data_types }
    }

    pub fn fns_impls(&self) -> impl Iterator<Item=&Fn> + '_ {
        self.fns.iter()
    }

    pub fn fns_impls_mut(&mut self) -> impl Iterator<Item=&mut Fn> + '_ {
        self.fns.iter_mut()
    }

    pub fn imports(&self) -> impl Iterator<Item=&Signature> + '_ {
        self.imports.iter()
    }

    pub fn data_types(&self) -> impl Iterator<Item=&Type> + '_ {
        self.data_types.iter()
    }

    pub fn add_lib(&mut self, lib: &dyn Library) {
        self.fns.extend(lib.fns());
        self.imports.extend(lib.imported_fns());
        self.data_types.extend(lib.data_types());
    }
}
