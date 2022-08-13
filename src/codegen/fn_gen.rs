
use std::ops::RangeFrom;

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{F64, I64};
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder};

use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::error::Result;
use crate::lexer;
use crate::parser::{self, Fn, Literal};



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

enum Value {
    Int(i64),
    Float(f64),
    String(DataId),
}

enum Expression {
    Call(FuncId, Vec<DataId>),
    Fn(String, Function),
    Let(Variable, Value),
    Literal(DataId),
}

pub struct FunctionCodegen<'gen> {
    fun_ctx: &'gen mut FunctionBuilderContext,
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    str_counter: Counter,
    var_counter: Counter,
}

impl<'gen> FunctionCodegen<'gen> {
    pub fn create_fn(
        fun_ctx: &'gen mut FunctionBuilderContext,
        data_ctx: &'gen mut DataContext,
        module: &'gen mut ObjectModule,
        path: &str,
        fun: &Fn,
    ) -> Result<Function> {
        let mut fun_codegen = Self {
            fun_ctx,
            data_ctx,
            module,
            str_counter: Counter::new(),
            var_counter: Counter::new(),
        };

        let func = fun_codegen.create_fn_imp(path, fun)?;
        Ok(func)
    }

    fn create_fn_imp(&mut self, path: &str, fun: &Fn) -> Result<Function> {
        let path = path.to_string() + fun.name();
        let pointer = self.module.target_config().pointer_type();

        let exprs: Vec<Expression> = fun
            .expressions()
            .map(|expr| match expr {
                parser::Expression::Call(call_name, params) => {
                    self.create_fn_call(&path, call_name, params)
                }
                parser::Expression::Fn(fun) => self
                    .create_fn_imp(&path, fun)
                    .map(|func| Expression::Fn(fun.name().to_string(), func)),
                parser::Expression::Let(id, value) => self
                    .create_variable(&path, value.clone())
                    .map(|(var, value)| Expression::Let(var, value)),
                parser::Expression::Literal(literal) => match literal {
                    Literal::String(s) => self
                        .create_literal_string(s.clone(), &path)
                        .map(|data| Expression::Literal(data)),
                    _ => unimplemented!(),
                },
            })
            .collect::<Result<_>>()?;

        let sig = self.module.make_signature();
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
        let mut builder = FunctionBuilder::new(&mut func, &mut self.fun_ctx);

        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        for expr in exprs {
            match expr {
                Expression::Call(id, params) => {
                    let callee = self.module.declare_func_in_func(id, builder.func);
                    let params = params
                        .into_iter()
                        .map(|data| {
                            let value = self.module.declare_data_in_func(data, builder.func);
                            builder.ins().symbol_value(pointer, value)
                        })
                        .collect::<Vec<_>>();
                    builder.ins().call(callee, &params);
                }
                Expression::Fn(name, func) => {
                    let mut ctx = Context::for_function(func);

                    let id =
                        self.module
                            .declare_function(&name, Linkage::Local, &ctx.func.signature)?;
                    self.module
                        .define_function(id, &mut ctx)?;
                }
                Expression::Let(var, value) => {
                    match value {
                        Value::Int(i) => {
                            builder.declare_var(var, I64);
                            let val = builder.ins().iconst(I64, i);
                            builder.def_var(var, val);
                        }
                        Value::Float(f) => {
                            builder.declare_var(var, F64);
                            let val = builder.ins().f64const(f);
                            builder.def_var(var, val);
                        }
                        Value::String(data) => {
                            builder.declare_var(var, pointer);
                            let value = self.module.declare_data_in_func(data, builder.func);
                            let val = builder.ins().symbol_value(pointer, value);
                            builder.def_var(var, val);
                        }
                    }
                }
                _ => (),
            }
        }

        builder.ins().return_(&[]);
        builder.finalize();

        verify_function(&func, self.module.isa())?;
        println!("{}", func.display());

        Ok(func)
    }

    fn create_fn_call(
        &mut self,
        path: &str,
        call_name: &str,
        params: &Vec<parser::Expression>,
    ) -> Result<Expression> {
        let pointer = self.module.target_config().pointer_type();

        let sig = {
            let mut sig = self.module.make_signature();
            for _ in 0..params.len() {
                sig.params.push(AbiParam::new(pointer));
            }
            sig
        };

        let callee = self
            .module
            .declare_function(&call_name, Linkage::Import, &sig)?;

        let mut args = Vec::new();
        for param in params.into_iter() {
            let str = match param {
                parser::Expression::Literal(Literal::String(s)) => s.clone(),
                _ => unimplemented!(),
            };

            let global = self.create_literal_string(str, path)?;
            args.push(global);
        }
        Ok(Expression::Call(callee, args))
    }

    fn create_variable(
        &mut self,
        path: &str,
        value: lexer::Value,
    ) -> Result<(Variable, Value)> {
        let _pointer = self.module.target_config().pointer_type();
        let var = Variable::new(self.var_counter.next());
        let value = match value {
            lexer::Value::Int(i) => Value::Int(i),
            lexer::Value::Float(f) => Value::Float(f),
            lexer::Value::String(s) => {
                let data = self.create_literal_string(s, &path)?;
                Value::String(data)
            }
        };

        Ok((var, value))
    }

    fn create_literal_string(&mut self, mut str: String, path: &str) -> Result<DataId> {
        str.push('\0');

        let name = path.to_string() + "_string_" + &self.str_counter.next().to_string();

        self.data_ctx
            .define(str.as_bytes().to_vec().into_boxed_slice());
        let id = self
            .module
            .declare_data(&name, Linkage::Local, true, false)?;
        self.module.define_data(id, &self.data_ctx)?;
        self.data_ctx.clear();
        Ok(id)
    }
}
