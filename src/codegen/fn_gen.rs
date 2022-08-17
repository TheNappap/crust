
use std::collections::HashMap;

use std::ops::RangeFrom;

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I64};
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder};

use cranelift_codegen::verifier::verify_function;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::error::Result;
use crate::lexer;
use crate::parser::{self, Fn, Literal, Type};


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

#[derive(Debug)]
enum Value {
    Int(i64),
    Float(f64),
    String(DataId),
}

#[derive(Debug)]
enum Expression {
    Call(FuncId, Vec<Expression>),
    Fn(String, Function),
    Let(Variable, Box<Expression>),
    Literal(Value),
    Symbol(String, Type),
}

pub struct FunctionCodegen<'gen> {
    fun_ctx: &'gen mut FunctionBuilderContext,
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    variables: HashMap<String, Variable>,
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
            variables: HashMap::new(),
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
                parser::Expression::Call(call_name, params, returns) => {
                    self.create_fn_call(&path, call_name, params, returns)
                }
                parser::Expression::Fn(fun) => self
                    .create_fn_imp(&path, fun)
                    .map(|func| Expression::Fn(fun.name().to_string(), func)),
                parser::Expression::Let(id, value) => self
                    .create_variable(&path, id.clone(), value.clone())
                    .map(|(var, expr)| Expression::Let(var, Box::new(expr))),
                parser::Expression::Literal(literal) => match literal {
                    Literal::String(s) => self
                        .create_literal_string(s.clone(), &path)
                        .map(|data| Expression::Literal(Value::String(data))),
                    _ => todo!(),
                },
                parser::Expression::Pointer(literal) => match literal {
                    Literal::Int(i) => self
                        .create_pointer_to_int(*i, &path)
                        .map(|data| Expression::Literal(Value::String(data))),
                    _ => todo!(),
                },
                parser::Expression::Symbol(name,ty) => {
                    Ok(Expression::Symbol(name.clone(), ty.clone()))
                },
            })
            .collect::<Result<_>>()?;

        let sig = self.module.make_signature();
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
        let mut builder = FunctionBuilder::new(&mut func, &mut self.fun_ctx);

        let block = builder.create_block();
        builder.switch_to_block(block);
        builder.seal_block(block);

        fn process_value(builder: &mut FunctionBuilder, module: &ObjectModule, variables: &HashMap<String,Variable>, expr: Expression) -> Vec<cranelift_codegen::ir::Value> {
            match expr {
                Expression::Literal(value) => match value {
                    Value::Int(i) => vec![builder.ins().iconst(I64, i)],
                    Value::Float(f) => vec![builder.ins().f64const(f)],
                    Value::String(data) => {
                        let value = module.declare_data_in_func(data, builder.func);
                        let pointer = module.target_config().pointer_type();
                        vec![builder.ins().symbol_value(pointer, value)]
                    }
                }
                Expression::Call(id, params) => {
                    let callee = module.declare_func_in_func(id, builder.func);
                    let params = params
                        .into_iter()
                        .map(|expr: Expression| {
                            process_value(builder, &module, variables, expr)[0]
                        })
                        .collect::<Vec<_>>();
                    let inst = builder.ins().call(callee, &params);
                    builder.inst_results(inst).to_vec()
                }
                Expression::Symbol(name, _) => {
                    let var = variables.get(&name).unwrap();
                    vec![builder.use_var(*var)]
                }
                _ => todo!(),
            }
        }

        for expr in exprs {
            match expr {
                Expression::Call(id, params) => {
                    let callee = self.module.declare_func_in_func(id, builder.func);
                    let params = params
                        .into_iter()
                        .map(|value| {
                            println!("{:?}", value);
                            process_value(&mut builder, &self.module, &self.variables, value)[0]
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
                Expression::Let(var, expr) => {
                    builder.declare_var(var, pointer);
                    let value = process_value(&mut builder, &self.module, &self.variables,*expr);
                    builder.def_var(var, value[0])
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
        returns: &Vec<Type>,
    ) -> Result<Expression> {
        let pointer = self.module.target_config().pointer_type();

        let sig = {
            let mut sig = self.module.make_signature();
            for _ in 0..params.len() {
                sig.params.push(AbiParam::new(pointer));
            }
            for _ in 0..returns.len() {
                sig.returns.push(AbiParam::new(pointer));
            }
            sig
        };

        let callee = self
            .module
            .declare_function(&call_name, Linkage::Import, &sig)?;
        
        let mut args = Vec::new();
        for param in params.into_iter() {
            let value = match param {
                parser::Expression::Literal(literal) => {
                    let value = match literal {
                        Literal::Int(i) => Value::Int(*i),
                        Literal::Float(f) => Value::Float(*f),
                        Literal::String(s) => Value::String(self.create_literal_string(s.to_string(), path)?),
                    };
                    Expression::Literal(value)
                },
                parser::Expression::Pointer(literal) => {
                    let value = match literal {
                        Literal::Int(i) => Value::String(self.create_pointer_to_int(*i, path)?),
                        Literal::Float(f) => Value::Float(*f),
                        Literal::String(s) => Value::String(self.create_literal_string(s.to_string(), path)?),
                    };
                    Expression::Literal(value)
                },
                parser::Expression::Call(name, params, returns) => {
                    let path = path.to_string()+name;
                    self.create_fn_call(&path, name, params, returns)?
                },
                parser::Expression::Symbol(name, ty) => {
                    Expression::Symbol(name.clone(), ty.clone())
                }
                _ => todo!(),
            };

            args.push(value);
        }
        Ok(Expression::Call(callee, args))
    }

    fn create_variable(
        &mut self,
        path: &str,
        name: String,
        value: lexer::Value,
    ) -> Result<(Variable, Expression)> {
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

        self.variables.insert(name, var);
        Ok((var, Expression::Literal(value)))
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

    fn create_pointer_to_int(&mut self, i: i64, path: &str) -> Result<DataId> {
        let name = path.to_string() + "_int_" + &self.str_counter.next().to_string();

        self.data_ctx
            .define(i.to_ne_bytes().to_vec().into_boxed_slice());
        let id = self
            .module
            .declare_data(&name, Linkage::Local, true, false)?;
        self.module.define_data(id, &self.data_ctx)?;
        self.data_ctx.clear();
        Ok(id)
    }
}
