use crate::{parser::{Library, Fn, Type}};

fn std_data_types() -> Vec<Type> {
    vec![
        Type::Struct("Range".to_owned(), [("start".to_owned(), Type::Int),("end".to_owned(), Type::Int)].into_iter().collect())
    ]
}

pub struct StdLib {
    fns: Vec<Fn>,
    data_types: Vec<Type>,
}

impl StdLib {
    pub fn new() -> Self {
        StdLib { fns: Vec::new(), data_types: std_data_types() }
    }
}

impl Library for StdLib {
    fn fns(&self) -> Vec<Fn> {
        self.fns.iter().cloned().collect()
    }

    fn data_types(&self) -> Vec<Type> {
        self.data_types.iter().cloned().collect()
    }
}