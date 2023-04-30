use self::code_generator::Codegen;
use crate::{
    error::{Error, Result, ErrorKind},
    parser::SyntaxTree, lexer::Position,
};
use cranelift_codegen::verifier::VerifierErrors;
use cranelift_module::ModuleError;
use std::{fs::File, io::Write};

mod code_generator;
mod fn_gen;
mod gen_type;
mod comp_kind;

pub fn build(syntax_tree: SyntaxTree) -> Result<()> {
    let mem = Codegen::new()?
        .compile(syntax_tree)?
        .emit()
        .map_err(|err|Error::new(ErrorKind::Codegen, err.to_string(), Position::zero()))?;

    File::create("main.o")
        .and_then(|mut file| file.write_all(&mem))
        .map_err(|err|Error::new(ErrorKind::Codegen, err.to_string(), Position::zero()))?;
    Ok(())
}

impl From<ModuleError> for Error {
    fn from(err: ModuleError) -> Error {
        Error::new(ErrorKind::Codegen, err.to_string(), Position::zero())
    }
}

impl From<VerifierErrors> for Error {
    fn from(err: VerifierErrors) -> Error {
        Error::new(ErrorKind::Codegen, err.to_string(), Position::zero())
    }
}

impl From<&str> for Error {
    fn from(err: &str) -> Error {
        Error::new(ErrorKind::Codegen, err.to_string(), Position::zero())
    }
}
