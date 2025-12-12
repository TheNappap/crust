use std::fmt;

use crate::error::{ThrowablePosition, ErrorKind, Error};


#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Position {
    row: u32,
    col: u32,
}

impl Position {
    pub fn zero() -> Self {
        Position { row: 0, col: 0 }
    }
    
    #[allow(dead_code)]
    pub fn new(row: u32, col: u32) -> Self {
        Position { row, col }
    }

    pub fn row(&self) -> u32 {
        self.row
    }

    pub fn col(&self) -> u32 {
        self.col
    }

    pub fn new_line(&mut self) {
        self.row += 1;
        self.col = 0;
    }

    pub fn add_col(&mut self) {
        self.col += 1;
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl ThrowablePosition for Position {
    fn error(&self, kind: ErrorKind, message: String) -> Error {
        Error::new(kind, message, self.clone())
    }
}

#[derive(PartialEq, Clone)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    pub fn union(&self, span: &Span) -> Self {
        let start = self.start.clone().min(span.start.clone());
        let end = self.end.clone().max(span.end.clone());
        Span { start, end }
    }

    pub fn start(&self) -> Position {
        self.start.clone()
    }

    pub fn end(&self) -> Position {
        self.end.clone()
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}->{:?}", self.start, self.end)
    }
}

impl ThrowablePosition for Span {
    fn error(&self, kind: ErrorKind, message: String) -> Error {
        Error::new(kind, message, self.start.clone())
    }
}