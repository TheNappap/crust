use std::ops::RangeFrom;

use cranelift_codegen::binemit::NullTrapSink;
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder};

use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::FunctionBuilder;
use cranelift_module::{DataId, FuncId, Linkage, Module};

use crate::error::Result;
use crate::parser::{self, Fn, Literal};

use super::code_generator::Codegen;

struct Counter {
    iter: RangeFrom<usize>,
}

impl Counter {
    fn new() -> Self {
        Counter { iter: 0.. }
    }

    fn next(&mut self) -> usize {
        self.iter.next().unwrap_or(0)
    }
}

enum Expression {
    Call(FuncId, Vec<DataId>),
    Fn(String, Function),
    Literal(DataId),
}

pub struct FunctionCodegen<'gen> {
    codegen: &'gen mut Codegen,
    counter: Counter,
}

impl<'gen> FunctionCodegen<'gen> {
    pub fn new(codegen: &'gen mut Codegen) -> Self {
        Self {
            codegen,
            counter: Counter::new(),
        }
    }

    pub fn create_fn(&mut self, path: &str, fun: &Fn) -> Result<Function> {
        let path = path.to_string() + fun.name();
        let exprs: Vec<Expression> = fun
            .expressions()
            .map(|expr| match expr {
                parser::Expression::Call(call_name, params) => {
                    self.create_fn_call(&path, call_name, params)
                }
                parser::Expression::Fn(fun) => self
                    .create_fn(&path, fun)
                    .map(|func| Expression::Fn(fun.name().to_string(), func)),
                parser::Expression::Literal(literal) => self
                    .create_literal_string(literal, &path)
                    .map(|data| Expression::Literal(data))
            })
            .collect::<Result<_>>()?;

        let pointer = self.codegen.module.target_config().pointer_type();

        let sig = self.codegen.module.make_signature();
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
        let mut builder = FunctionBuilder::new(&mut func, &mut self.codegen.fun_ctx);
        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.seal_block(block);

        for expr in exprs {
            match expr {
                Expression::Call(id, params) => {
                    let callee = self.codegen.module.declare_func_in_func(id, builder.func);
                    let params = params
                        .into_iter()
                        .map(|data| {
                            let value =
                                self.codegen.module.declare_data_in_func(data, builder.func);
                            builder.ins().symbol_value(pointer, value)
                        })
                        .collect::<Vec<_>>();
                    builder.ins().call(callee, &params);
                }
                Expression::Fn(name, func) => {
                    let mut ctx = Context::for_function(func);

                    let id = self.codegen.module.declare_function(
                        &name,
                        Linkage::Local,
                        &ctx.func.signature,
                    )?;
                    self.codegen
                        .module
                        .define_function(id, &mut ctx, &mut NullTrapSink {})?;
                }
                _ => (),
            }
        }

        builder.ins().return_(&[]);
        builder.finalize();

        verify_function(&func, self.codegen.module.isa())?;
        println!("{}", func.display(self.codegen.module.isa()));
        Ok(func)
    }

    fn create_fn_call(
        &mut self,
        path: &str,
        call_name: &str,
        params: &Vec<parser::Expression>,
    ) -> Result<Expression> {
        let pointer = self.codegen.module.target_config().pointer_type();

        let sig = {
            let mut sig = self.codegen.module.make_signature();
            for _ in 0..params.len() {
                sig.params.push(AbiParam::new(pointer));
            }
            sig
        };

        let callee = self
            .codegen
            .module
            .declare_function(&call_name, Linkage::Import, &sig)?;

        let mut args = Vec::new();
        for param in params.into_iter() {
            let literal = match param {
                parser::Expression::Literal(literal) => literal,
                _ => unimplemented!(),
            };

            let global = self.create_literal_string(literal, path)?;
            args.push(global);
        }
        Ok(Expression::Call(callee, args))
    }

    fn create_literal_string(&mut self, literal: &Literal, path: &str) -> Result<DataId> {
        let str = match literal {
            Literal::String(str) => str,
            _ => unimplemented!(),
        };
        let mut str = str.clone();
        str.push('\0');
        
        let name = path.to_string() + "_string_" + &self.counter.next().to_string();

        self.codegen
            .data_ctx
            .define(str.as_bytes().to_vec().into_boxed_slice());
        let id = self
            .codegen
            .module
            .declare_data(&name, Linkage::Local, true, false)?;
        self.codegen
            .module
            .define_data(id, &self.codegen.data_ctx)?;
        self.codegen.data_ctx.clear();
        Ok(id)
    }
}
