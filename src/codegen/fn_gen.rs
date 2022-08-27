
use std::collections::HashMap;

use std::ops::RangeFrom;

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I64, F64};
use cranelift_codegen::ir::{AbiParam, ExternalName, Function, InstBuilder, Value, StackSlotData, StackSlotKind, FuncRef, Block, self};

use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::ObjectModule;

use crate::error::{Result, Error};
use crate::lexer::Literal;
use crate::parser::{Fn, Type, Expression, Signature};

pub fn create_fn<'gen>(
    fun_ctx: &'gen mut FunctionBuilderContext,
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    path: &str,
    fun: &Fn,
) -> Result<Function> {
    let mut sig = module.make_signature();
    for param in fun.signature().params() {
        if let Some(ty) = get_type(param, module) {
            sig.params.push(AbiParam::new(ty))
        }
    }
    if let Some(ty) = get_type(fun.signature().returns(), module) {
        sig.returns.push(AbiParam::new(ty))
    }

    let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
    let builder = FunctionBuilder::new(&mut func, fun_ctx);

    let mut fun_codegen = FunctionCodegen {
        data_ctx,
        module,
        builder,
        path: path.to_string() + fun.signature().name(),
        variables: HashMap::new(),
        functions: HashMap::new(),
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
    functions: HashMap<String, FuncRef>,
    str_counter: Counter,
    var_counter: Counter,
}

impl<'gen> FunctionCodegen<'gen> {
    fn create_fn(&mut self, fun: &Fn) -> Result<()> {
        let block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(block);
        
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);

        for (index, (name, ty)) in fun.params().zip(fun.signature().params()).enumerate() {
            self.create_parameter(name.clone(), index, ty, block)?;
        }

        for statement in fun.expressions() {
            self.create_expression(statement)?;
        }

        if *fun.signature().returns() == Type::Void {
            self.builder.ins().return_(&[]);
        }
        self.builder.finalize();
        Ok(())
    }

    
    fn create_expression(&mut self, expression: &Expression) -> Result<Value> {
        let value = match expression {
            Expression::Call(signature, params) => {
                self.create_fn_call(signature, params)?
            }
            Expression::Return(expr) => {
                self.create_return(expr)?
            }
            Expression::Let(id, expr, ty) => {
                self.create_local_variable(id.clone(), expr, ty)?
            }
            Expression::If(condition, body) => {
                self.create_if(condition, body)?
            }
            Expression::Literal(literal) => match literal {
                Literal::Int(i) => self.builder.ins().iconst(I64, *i),
                Literal::Float(f) => self.builder.ins().f64const(*f),
                Literal::Bool(b) => self.builder.ins().iconst(I64, *b),
                Literal::String(s) => self.create_literal_string(s.clone())?
            }
            Expression::AddrOf(expression) => {
                let value = self.create_expression(expression)?;
                self.create_pointer_to_stack_slot(value)?
            }
            Expression::Symbol(name, _) => {
                let var = self.variables.get(name).unwrap();
                self.builder.use_var(*var)
            }
            Expression::Add(param1, param2, ty) => {
                self.create_addition(param1, param2, ty)?
            }
            Expression::Sub(param1, param2, ty) => {
                self.create_subtraction(param1, param2, ty)?
            }
            Expression::Mul(param1, param2, ty) => {
                self.create_multiplication(param1, param2, ty)?
            }
            Expression::Div(param1, param2, ty) => {
                self.create_division(param1, param2, ty)?
            }
            Expression::Fn(_) => Value::from_u32(0), //ignore, handled before function codegen
        };
        Ok(value)
    }

    fn create_fn_call(
        &mut self,
        signature: &Signature,
        params: &[Expression],
    ) -> Result<Value> {
        let callee = if let Some(f) = self.functions.get(signature.name()) {
            *f
        } else {
            let sig = {
                let mut sig = self.module.make_signature();
                for param_type in signature.params() {
                    if let Some(ty) = get_type(param_type, self.module) {
                        sig.params.push(AbiParam::new(ty));
                    }
                }
                if let Some(ty) = get_type(signature.returns(), self.module) {
                    sig.returns.push(AbiParam::new(ty))
                }
                sig
            };

            let id = self
                .module
                .declare_function(signature.name(), Linkage::Import, &sig)?;
        
            let callee = self.module.declare_func_in_func(id, self.builder.func);
            self.functions.insert(signature.name().to_string(), callee);
            callee
        };

        let mut param_values = Vec::new();
        for param in params.iter() {
            param_values.push(self.create_expression(param)?);
        }

        let inst = self.builder.ins().call(callee, &param_values);
        let return_values = self.builder.inst_results(inst).to_vec();
        Ok(*return_values.first().unwrap_or(&Value::from_u32(0)))
    }

    fn create_return(
        &mut self,
        expr: &Expression
    ) -> Result<Value> {
        let value = self.create_expression(expr)?;
        self.builder.ins().return_(&[value]);
        Ok(value)
    }

    fn create_parameter(
        &mut self,
        name: String,
        index: usize,
        ty: &Type,
        block: Block,
    ) -> Result<Value> {
        let value = self.builder.block_params(block)[index];
        self.create_variable(name, value, ty)
    }
    
    fn create_local_variable(
        &mut self,
        name: String,
        expr: &Expression,
        ty: &Type
    ) -> Result<Value> {
        let value = self.create_expression(expr)?;
        self.create_variable(name, value, ty)
    }

    fn create_variable(
        &mut self,
        name: String,
        value: Value,
        ty: &Type
    ) -> Result<Value> {
        let ty = get_type(ty, self.module)
                .ok_or_else(|| Error::codegen("Unexpected type for variable".to_string(), 0))?;
        
        let var = Variable::new(self.var_counter.next());

        self.builder.declare_var(var, ty);
        self.builder.def_var(var, value);
        self.variables.insert(name, var);
        Ok(value)
    }

    fn create_if(&mut self, condition: &Expression, body: &[Expression]) -> Result<Value> {
        let if_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        let cond = self.create_expression(condition)?;
        self.builder.ins().brz(cond, after_block, &[]);
        self.builder.ins().jump(if_block, &[]);

        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);

        for statement in body {
            self.create_expression(statement)?;
        }
        
        self.builder.ins().jump(after_block, &[]);
        
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        Ok(Value::from_u32(0))
    }

    fn create_pointer_to_stack_slot(&mut self, value: Value) -> Result<Value> {
        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 8));
        self.builder.ins().stack_store(value, stack_slot, 0);
        Ok(self.builder.ins().stack_addr(I64, stack_slot, 0))
    }

    fn create_literal_string(&mut self, mut str: String) -> Result<Value> {
        str.push('\0');
        let name = self.path.clone() + "_string_" + &self.str_counter.next().to_string();
        let contents = str.as_bytes().to_vec().into_boxed_slice();

        self.create_data(&name, contents)
    }
    
    fn create_data(&mut self, name: &str, contents: Box<[u8]>) -> Result<Value> {
        self.data_ctx.define(contents);
        let data = self
            .module
            .declare_data(name, Linkage::Local, true, false)?;
        self.module.define_data(data, self.data_ctx)?;
        self.data_ctx.clear();

        
        let global_value = self.module.declare_data_in_func(data, self.builder.func);
        let pointer = self.module.target_config().pointer_type();
        let value = self.builder.ins().symbol_value(pointer, global_value);
        Ok(value)
    }

    fn create_addition(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Value> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().iadd(v1, v2))
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().fadd(v1, v2))
            },
            Type::String => self.create_fn_call(&Signature::new("strcat",vec![Type::Int,Type::Int],Type::Int), &vec![param1.clone(), param2.clone()]),
            _ => Err(Error::codegen("Addition for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_subtraction(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Value> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().isub(v1, v2))
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().fsub(v1, v2))
            },
            _ => Err(Error::codegen("Subtration for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_multiplication(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Value> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().imul(v1, v2))
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().fmul(v1, v2))
            },
            _ => Err(Error::codegen("Multiplication for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_division(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Value> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().sdiv(v1, v2))
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?;
                let v2 = self.create_expression(param2)?;
                Ok(self.builder.ins().fdiv(v1, v2))
            },
            _ => Err(Error::codegen("Division for this type is not supported".to_string(), 0))
        }
    }
}

fn get_type(ty: &Type, module: &ObjectModule) -> Option<ir::Type> {
    let ty = match ty {
        Type::Int => I64,
        Type::Float => F64,
        Type::Bool => I64,
        Type::String => module.target_config().pointer_type(),
        Type::Void | Type::Inferred => return None,
    };
    Some(ty)
}