
use std::collections::HashMap;

use std::ops::RangeFrom;

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I64};
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Value};

use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, DataId, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::error::Result;
use crate::parser::{Fn, Literal, Type, Expression};

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
        path: path.to_string() + fun.name(),
        variables: HashMap::new(),
        str_counter: Counter::new(),
        var_counter: Counter::new(),
    };

    fun_codegen.create_fn(fun)?;

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

struct FunctionCodegen<'gen> {
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    builder: FunctionBuilder<'gen>,
    path: String,
    variables: HashMap<String, Variable>,
    str_counter: Counter,
    var_counter: Counter,
}

impl<'gen> FunctionCodegen<'gen> {
    fn create_fn(&mut self, fun: &Fn) -> Result<()> {
        let block = self.builder.create_block();
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);

        for statement in fun.expressions() {
            self.create_expression(statement)?;
        }

        self.builder.ins().return_(&[]);
        self.builder.finalize();
        Ok(())
    }

    
    fn create_expression(&mut self, expression: &Expression) -> Result<Value> {
        match expression {
            Expression::Call(call_name, params, returns) => {
                self.create_fn_call(&call_name, &params, &returns)
            }
            Expression::Let(id, expr) => {
                self.create_variable(id.clone(), expr)
            }
            Expression::Literal(literal) => Ok(match literal {
                Literal::Int(i) => self.builder.ins().iconst(I64, *i),
                Literal::Float(f) => self.builder.ins().f64const(*f),
                Literal::String(s) => {
                    let data = self.create_literal_string(s.clone())?;
                
                    let value = self.module.declare_data_in_func(data, self.builder.func);
                    let pointer = self.module.target_config().pointer_type();
                    self.builder.ins().symbol_value(pointer, value)
                }
            }),
            Expression::Pointer(literal) => Ok(match literal {
                Literal::Int(i) => { 
                    let data = self.create_pointer_to_int(*i)?;
                    //.map(|data| Expression::Literal(Value::String(data)))
                    let value = self.module.declare_data_in_func(data, self.builder.func);
                    let pointer = self.module.target_config().pointer_type();
                    self.builder.ins().symbol_value(pointer, value)
                },
                Literal::String(s) => {
                    let data = self.create_literal_string(s.clone())?;
                    //.map(|data| Expression::Literal(Value::String(data)))
                    let value = self.module.declare_data_in_func(data, self.builder.func);
                    let pointer = self.module.target_config().pointer_type();
                    self.builder.ins().symbol_value(pointer, value)
                },
                _ => todo!(),
            }),
            Expression::Symbol(name, _) => {
                let var = self.variables.get(name).unwrap();
                Ok(self.builder.use_var(*var))
            }
            Expression::Fn(_) => Ok(Value::from_u32(0)), //ignore, handled before function codegen
        }
    }

    fn create_fn_call(
        &mut self,
        call_name: &str,
        params: &Vec<Expression>,
        returns: &Vec<Type>,
    ) -> Result<Value> {
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

        let mut param_values = Vec::new();
        for param in params.into_iter() {
            param_values.push(self.create_expression(param)?);
        }

        let id = self
            .module
            .declare_function(&call_name, Linkage::Import, &sig)?;
        
        let callee = self.module.declare_func_in_func(id, self.builder.func);

        let inst = self.builder.ins().call(callee, &param_values);
        let return_values = self.builder.inst_results(inst).to_vec();
        Ok(*return_values.first().unwrap_or(&Value::from_u32(0)))
    }

    fn create_variable(
        &mut self,
        name: String,
        expr: &Expression,
    ) -> Result<Value> {
        let pointer = self.module.target_config().pointer_type();
        let var = Variable::new(self.var_counter.next());
        let value = self.create_expression(expr)?;

        self.builder.declare_var(var, pointer);
        self.builder.def_var(var, value);
        self.variables.insert(name, var);
        Ok(value)
    }

    fn create_literal_string(&mut self, mut str: String) -> Result<DataId> {
        str.push('\0');

        let name = self.path.clone() + "_string_" + &self.str_counter.next().to_string();

        self.data_ctx
            .define(str.as_bytes().to_vec().into_boxed_slice());
        let id = self
            .module
            .declare_data(&name, Linkage::Local, true, false)?;
        self.module.define_data(id, &self.data_ctx)?;
        self.data_ctx.clear();
        Ok(id)
    }

    fn create_pointer_to_int(&mut self, i: i64) -> Result<DataId> {
        let name = self.path.clone() + "_int_" + &self.str_counter.next().to_string();

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
