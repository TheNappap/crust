use crate::lexer::Position;

macro_rules! try_option {
    ($x:expr) => {
        match $x {
            Ok(Some(value)) => value,
            Ok(None) => return Ok(None),
            Err(error) => return Err(error.into()),
        }
    }
}

pub(crate) use try_option;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Lexer,
    Syntax,
    Type,
    Codegen,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Lexer => write!(f, "Lexer"),
            ErrorKind::Syntax => write!(f, "Syntax"),
            ErrorKind::Type => write!(f, "Type"),
            ErrorKind::Codegen => write!(f, "Codegen"),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    pos: Position,
}

impl Error {
    pub fn new(kind: ErrorKind, message: String, pos: Position) -> Self {
        Error { kind, message, pos }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} Error:{}:{}: {}",
            self.kind, self.pos.row(), self.pos.col(), self.message
        )
    }
}

pub trait ThrowablePosition {
    fn error(&self, kind: ErrorKind, message: String) -> Error;
    fn throw<T>(&self, kind: ErrorKind, message: String) -> Result<T> {
        Err(self.error(kind, message))
    }
    fn lexer<T>(&self, message: String) -> Result<T> {
        self.throw(ErrorKind::Lexer, message)
    }
    fn syntax<T>(&self, message: String) -> Result<T> {
        self.throw(ErrorKind::Syntax, message)
    }
    fn type_<T>(&self, message: String) -> Result<T> {
        self.throw(ErrorKind::Type, message)
    }
    fn codegen<T>(&self, message: String) -> Result<T> {
        self.throw(ErrorKind::Codegen, message)
    }
}

