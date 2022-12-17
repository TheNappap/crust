use self::code_generator::Codegen;
use crate::{
    error::{Error, Result},
    parser::SyntaxTree,
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
        .map_err(|err| err.to_string())?;
    File::create("main.o")?.write_all(&mem)?;
    Ok(())
}

impl From<ModuleError> for Error {
    fn from(err: ModuleError) -> Error {
        Error::codegen(err.to_string(), 0)
    }
}

impl From<VerifierErrors> for Error {
    fn from(err: VerifierErrors) -> Error {
        Error::codegen(err.to_string(), 0)
    }
}
