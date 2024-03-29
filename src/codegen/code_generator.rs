
use cranelift_codegen::settings::Flags;
use cranelift_codegen::{settings, Context, CodegenError};
use cranelift_frontend::{FunctionBuilderContext};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};

use crate::error::{Result, Error, ErrorKind};
use crate::lexer::Position;
use crate::parser::{SyntaxTree, Fn, ExpressionKind};

impl From<CodegenError> for Error {
    fn from(e: CodegenError) -> Self {
        Error::new(ErrorKind::Codegen, format!("{}", e), Position::zero())
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
        for fun in syntax_tree.fns_impls() {
            for expr in fun.body() {
                if let ExpressionKind::Fn(f) = &expr.kind {
                    self.compile_fn(f, fun.signature().name())?;
                }
            }

            self.compile_fn(fun, "")?;
        }
        Ok(self.module.finish())
    }

    pub fn compile_fn(&mut self, fun: &Fn, path: &str) -> Result<()> {
        let func = super::fn_gen::create_fn(
            &mut self.fun_ctx,
            &mut self.data_ctx,
            &mut self.module,
            path,
            fun,
        )?;

        let mut ctx = Context::for_function(func);
        let id =
            self.module
                .declare_function(fun.signature().name(), Linkage::Export, &ctx.func.signature)?;
        self.module
            .define_function(id, &mut ctx)?;
        Ok(())
    }
}
