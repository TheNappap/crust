
use cranelift_codegen::settings::{Configurable, Flags};
use cranelift_codegen::{settings, Context, CodegenError};
use cranelift_frontend::{FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

use crate::utils::{Result, Error, ErrorKind};
use crate::lexer::Position;
use crate::parser::{SyntaxTree, Fn};

impl From<CodegenError> for Error {
    fn from(e: CodegenError) -> Self {
        Error::new(ErrorKind::Codegen, format!("{}", e), Position::zero())
    }
}

pub struct Codegen {
    pub fun_ctx: FunctionBuilderContext,
    pub data_ctx: DataDescription,
    pub module: ObjectModule,
}

impl Codegen {
    pub fn new() -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("enable_multi_ret_implicit_sret", "true").unwrap();
        let shared_flags = Flags::new(flag_builder);
        let isa = cranelift_native::builder()?.finish(shared_flags)?;
        let builder = ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names())?;
        let module = ObjectModule::new(builder);
        Ok(Self {
            fun_ctx: FunctionBuilderContext::new(),
            data_ctx: DataDescription::new(),
            module,
        })
    }

    pub fn compile(mut self, syntax_tree: SyntaxTree) -> Result<ObjectProduct> {
        for fun in syntax_tree.hidden_fns_impls() {
            self.compile_fn(fun, Linkage::Local, "")?; // TODO: add path for hidden fns
        }
        for fun in syntax_tree.fns_impls() {
            self.compile_fn(fun, Linkage::Export, "")?;
        }
        Ok(self.module.finish())
    }

    pub fn compile_fn(&mut self, fun: &Fn, linkage: Linkage, path: &str) -> Result<()> {
        //println!("Compiling: {:?}", fun.signature().name());
        let func = super::fn_gen::create_fn(
            &mut self.fun_ctx,
            &mut self.data_ctx,
            &mut self.module,
            path,
            fun,
        )?;

        let mut ctx = Context::for_function(func);
        let id = self.module.declare_function(fun.signature().name(), linkage, &ctx.func.signature)?;
        self.module.define_function(id, &mut ctx)?;
        Ok(())
    }
}
