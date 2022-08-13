
use cranelift_codegen::settings::Flags;
use cranelift_codegen::{settings, Context, CodegenError};
use cranelift_frontend::{FunctionBuilderContext};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

use crate::error::{Result, Error};
use crate::parser::SyntaxTree;

use super::fn_gen::FunctionCodegen;

impl From<CodegenError> for Error {
    fn from(e: CodegenError) -> Self {
        Error::codegen(format!("{}", e),0)
    }
}

pub struct Codegen {
    pub fun_ctx: FunctionBuilderContext,
    pub data_ctx: DataContext,
    pub module: ObjectModule,
}

impl Codegen {
    pub fn new() -> Result<Self> {
        let isa = cranelift_native::builder()?.finish(Flags::new(settings::builder()))?;
        let builder = ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names())?;
        let module = ObjectModule::new(builder);
        Ok(Self {
            fun_ctx: FunctionBuilderContext::new(),
            data_ctx: DataContext::new(),
            module,
        })
    }

    pub fn compile(mut self, syntax_tree: SyntaxTree) -> Result<ObjectProduct> {
        for fun in syntax_tree.fns() {
            let func = FunctionCodegen::create_fn(
                &mut self.fun_ctx,
                &mut self.data_ctx,
                &mut self.module,
                "",
                fun,
            )?;

            let mut ctx = Context::for_function(func);
            let id =
                self.module
                    .declare_function(fun.name(), Linkage::Export, &ctx.func.signature)?;
            self.module
                .define_function(id, &mut ctx)?;
        }
        Ok(self.module.finish())
    }
}
