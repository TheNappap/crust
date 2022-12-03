use crate::{parser::{Data, Library, Fn, Type}};

fn std_data() -> Vec<Data> {
    vec![
        Data::new("Range".to_owned(), Type::Struct([("start".to_owned(), Type::Int),("end".to_owned(), Type::Int)].into_iter().collect()))
    ]
}

pub struct StdLib {
    fns: Vec<Fn>,
    data: Vec<Data>,
}

impl StdLib {
    pub fn new() -> Self {
        StdLib { fns: Vec::new(), data: std_data() }
    }
}

impl Library for StdLib {
    fn fns(&self) -> Vec<Fn> {
        self.fns.iter().cloned().collect()
    }

    fn data(&self) -> Vec<Data> {
        self.data.iter().cloned().collect()
    }
}