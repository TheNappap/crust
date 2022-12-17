use crate::{parser::{Library, Fn, Type, Signature}};

fn std_data_types() -> Vec<Type> {
    vec![
        Type::Struct("Range".to_owned(), [("start".to_owned(), Type::Int),("end".to_owned(), Type::Int)].into_iter().collect())
    ]
}

fn import_functions() -> Vec<Signature> {
    vec![
        Signature::new(None, "__stdio_common_vfprintf",vec![Type::Int,Type::Int,Type::String,Type::Int,Type::Int],Type::Void),
        Signature::new(None, "__acrt_iob_func", vec![Type::Int], Type::Int)
    ]
}

pub struct StdLib {
    fns: Vec<Fn>,
    imported_fns: Vec<Signature>,
    data_types: Vec<Type>,
}

impl StdLib {
    pub fn new() -> Self {
        StdLib { fns: Vec::new(), imported_fns: import_functions(), data_types: std_data_types() }
    }
}

impl Library for StdLib {
    fn fns(&self) -> Vec<Fn> {
        self.fns.iter().cloned().collect()
    }

    fn imported_fns(&self) -> Vec<Signature> {
        self.imported_fns.iter().cloned().collect()
    }

    fn data_types(&self) -> Vec<Type> {
        self.data_types.iter().cloned().collect()
    }
}