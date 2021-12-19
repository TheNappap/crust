#[derive(Debug, Clone)]
pub enum ErrorKind {
    Lexer,
    Syntax,
    Codegen,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Lexer => write!(f, "Lexer"),
            ErrorKind::Syntax => write!(f, "Syntax"),
            ErrorKind::Codegen => write!(f, "Codegen"),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    line: usize,
}

impl Error {
    pub fn lexer(message: String, line: usize) -> Error {
        Error {
            kind: ErrorKind::Lexer,
            message,
            line,
        }
    }

    pub fn syntax(message: String, line: usize) -> Error {
        Error {
            kind: ErrorKind::Syntax,
            message,
            line,
        }
    }

    pub fn codegen(message: String, line: usize) -> Error {
        Error {
            kind: ErrorKind::Codegen,
            message,
            line,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} Error (line {}): {}",
            self.kind, self.line, self.message
        )
    }
}

impl From<&str> for Error {
    fn from(err: &str) -> Error {
        Error::codegen(err.to_string(), 0)
    }
}

impl From<String> for Error {
    fn from(err: String) -> Error {
        Error::codegen(err, 0)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error::codegen(err.to_string(), 0)
    }
}
