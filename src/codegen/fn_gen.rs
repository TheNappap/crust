
use std::collections::HashMap;

use std::ops::RangeFrom;

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I64};
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder};

use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::error::Result;
use crate::lexer;
use crate::parser::{self, Fn, Literal, Type};

pub fn create_fn<'gen>(
    fun_ctx: &'gen mut FunctionBuilderContext,
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    path: &str,
    fun: &Fn,
) -> Result<Function> {
    let sig = module.make_signature();
    let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
    let builder = FunctionBuilder::new(&mut func, fun_ctx);

    let mut fun_codegen = FunctionCodegen {
        data_ctx,
        module,
        builder,
        variables: HashMap::new(),
        str_counter: Counter::new(),
        var_counter: Counter::new(),
    };

    fun_codegen.create_fn(path, fun)?;
    

    verify_function(&func, module.isa())?;
    println!("{}", func.display());

    Ok(func)
}


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
enum Expression {
    Call(String, Vec<parser::Expression>, Vec<Type>),
    Fn(String),
    Let(String, lexer::Value),
    Literal(Literal),
    Pointer(Literal),
    Symbol(String, Type),
}

struct FunctionCodegen<'gen> {
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    builder: FunctionBuilder<'gen>,
    variables: HashMap<String, Variable>,
    str_counter: Counter,
    var_counter: Counter,
}

impl<'gen> FunctionCodegen<'gen> {
    fn create_fn(&mut self, path: &str, fun: &Fn) -> Result<()> {
        let block = self.builder.create_block();
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);

        let path = path.to_string() + fun.name();
        for expr in fun.expressions() {
            self.create_expression(&path, expr)?
        }

        self.builder.ins().return_(&[]);
        self.builder.finalize();
        Ok(())
    }

    
    fn create_expression(&mut self, path: &str, expression: &parser::Expression) -> Result<()> {
        fn process_value(codegen: &mut FunctionCodegen, path: &str, expr: Expression) -> Result<Vec<cranelift_codegen::ir::Value>> {
            match expr {
                Expression::Literal(literal) => Ok(match literal {
                    Literal::Int(i) => vec![codegen.builder.ins().iconst(I64, i)],
                    Literal::Float(f) => vec![codegen.builder.ins().f64const(f)],
                    Literal::String(s) => {
                        let data = codegen.create_literal_string(s.clone(), &path)?;
                    
                        let value = codegen.module.declare_data_in_func(data, codegen.builder.func);
                        let pointer = codegen.module.target_config().pointer_type();
                        vec![codegen.builder.ins().symbol_value(pointer, value)]
                    }
                }),
                Expression::Pointer(literal) => Ok(match literal {
                    Literal::Int(i) => { 
                        let data = codegen.create_pointer_to_int(i, &path)?;
                        //.map(|data| Expression::Literal(Value::String(data)))
                        let value = codegen.module.declare_data_in_func(data, codegen.builder.func);
                        let pointer = codegen.module.target_config().pointer_type();
                        vec![codegen.builder.ins().symbol_value(pointer, value)]
                    },
                    Literal::String(s) => {
                        let data = codegen.create_literal_string(s.clone(), &path)?;
                        //.map(|data| Expression::Literal(Value::String(data)))
                        let value = codegen.module.declare_data_in_func(data, codegen.builder.func);
                        let pointer = codegen.module.target_config().pointer_type();
                        vec![codegen.builder.ins().symbol_value(pointer, value)]
                    },
                    _ => todo!(),
                }),
                Expression::Call(call_name, params, returns) => {
                    let (id, params) = codegen.create_fn_call(&path, &call_name, &params, &returns)?;
                    let callee = codegen.module.declare_func_in_func(id, codegen.builder.func);
                    let params = params
                        .into_iter()
                        .map(|expr: Expression| {
                            process_value(codegen, path, expr).map(|v|v[0])
                        })
                        .collect::<Result<Vec<_>>>()?;
                    let inst = codegen.builder.ins().call(callee, &params);
                    Ok(codegen.builder.inst_results(inst).to_vec())
                }
                Expression::Symbol(name, _) => {
                    let var = codegen.variables.get(&name).unwrap();
                    Ok(vec![codegen.builder.use_var(*var)])
                }
                _ => todo!(),
            }
        }

        match expression {
            parser::Expression::Call(call_name, params, returns) => {
                let (id, params) = self.create_fn_call(&path, &call_name, &params, &returns)?;
                let callee = self.module.declare_func_in_func(id, self.builder.func);
                let params = params
                    .into_iter()
                    .map(|value| {
                        println!("{:?}", value);
                        process_value(self, &path, value)
                            .map(|v| v[0])
                    })
                    .collect::<Result<Vec<_>>>()?;
                    self.builder.ins().call(callee, &params);
            }
            parser::Expression::Let(id, value) => {
                let (var, expr) = self
                    .create_variable(&path, id.clone(), value.clone())
                    .map(|(var, expr)| (var, expr))?;
                
                let pointer = self.module.target_config().pointer_type();
                self.builder.declare_var(var, pointer);
                let value = process_value(self, &path, expr)?;
                self.builder.def_var(var, value[0])
            }
            _ => (),
        }
        Ok(())
    }

    fn create_fn_call(
        &mut self,
        path: &str,
        call_name: &str,
        params: &Vec<parser::Expression>,
        returns: &Vec<Type>,
    ) -> Result<(FuncId, Vec<Expression>)> {
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
                    Expression::Literal(literal.clone())
                },
                parser::Expression::Pointer(literal) => {
                    // let value = match literal {
                    //     Literal::Int(i) => Value::String(self.create_pointer_to_int(*i, path)?),
                    //     Literal::Float(f) => Value::Float(*f),
                    //     Literal::String(s) => Value::String(self.create_literal_string(s.to_string(), path)?),
                    // }; //TODO
                    Expression::Literal(literal.clone())
                },
                parser::Expression::Call(name, params, returns) => {
                    let path = path.to_string()+name;
                    Expression::Call(name.clone(), params.clone(), returns.clone())
                },
                parser::Expression::Symbol(name, ty) => {
                    Expression::Symbol(name.clone(), ty.clone())
                }
                _ => todo!(),
            };

            args.push(value);
        }
        Ok((callee, args))
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
            lexer::Value::Int(i) => Literal::Int(i),
            lexer::Value::Float(f) => Literal::Float(f),
            lexer::Value::String(s) => Literal::String(s),
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
