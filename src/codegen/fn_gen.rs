
use std::collections::HashMap;

use std::ops::{RangeFrom, Range};

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I64, B64};
use cranelift_codegen::ir::{ExternalName, Function, InstBuilder, Value, StackSlotData, StackSlotKind, FuncRef, Block, InstBuilderBase, StackSlot, self, MemFlags};

use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::ObjectModule;
use itertools::Itertools;

use crate::error::{Result, Error};
use crate::lexer::Literal;
use crate::parser::{Fn, Expression, Signature, BinOpKind, UnOpKind, Type};

use super::comp_kind::CompKind;
use super::types::GenType;

pub fn create_fn<'gen>(
    fun_ctx: &'gen mut FunctionBuilderContext,
    data_ctx: &'gen mut DataContext,
    module: &'gen mut ObjectModule,
    path: &str,
    fun: &Fn,
) -> Result<Function> {
    let mut sig = module.make_signature();
    for param_type in fun.signature().params() {
        let ty = GenType::from_type(param_type, module)?;
        ty.add_to_params(&mut sig.params);
    }
    let ty = GenType::from_type(fun.signature().returns(), module)?;
    ty.add_to_params(&mut sig.returns);

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
    //println!("{}", func.display());

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
    variables: HashMap<String, StackSlot>,
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

        let mut offset = 0;
        for (name, ty) in fun.params() {
            let ty = GenType::from_type(ty, self.module)?;
            let new_offset = offset + ty.offsets().len();
            self.create_parameter(name.clone(), offset..new_offset, &ty, block)?;
            offset = new_offset;
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

    fn create_expression(&mut self, expression: &Expression) -> Result<Vec<Value>> {
        let value = match expression {
            Expression::Call(signature, params) => {
                self.create_fn_call(signature, params)?
            }
            Expression::Return(expr) => {
                self.create_return(expr)?
            }
            Expression::Let(id, expr, ty) => {
                self.create_local_variable(id.clone(), expr, &GenType::from_type(ty, self.module)?)?
            }
            Expression::Mut(id, expr) => {
                self.create_variable_mutation(id, expr)?
            }
            Expression::If(condition, if_body, else_body) => {
                self.create_if(condition, if_body, else_body)?
            }
            Expression::While(condition, while_body) => {
                self.create_while(condition, while_body)?
            }
            Expression::For(iter, var_name, var_type, for_body) => {
                self.create_for(iter, var_name.clone(), &GenType::from_type(var_type, self.module)?, for_body)?
            }
            Expression::Iter(iter, _) => {
                self.create_expression(iter)?
            }
            Expression::Index(collection, index, ty, coll_length) => {
                self.create_index(collection, index, &GenType::from_type(ty, self.module)?, *coll_length)?
            }
            Expression::Group(body) => {
                self.create_group(body)?
            }
            Expression::Literal(literal) => vec![match literal {
                Literal::Int(i) => self.builder.ins().iconst(I64, *i),
                Literal::Float(f) => self.builder.ins().f64const(*f),
                Literal::Bool(b) => self.builder.ins().bconst(B64, *b),
                Literal::String(s) => self.create_literal_string(s.clone())?
            }],
            Expression::AddrOf(expressions) => {
                let values: Vec<_> = expressions.iter()
                        .map(|expr|self.create_expression(expr))
                        .flatten_ok()
                        .try_collect()?;
                self.create_pointer_to_stack_slot(&values)?
            }
            Expression::Symbol(name, ty) => {
                self.create_symbol_expr(name, ty, 0)?
            }
            Expression::Field(var_name, _, ty, offset) => {
                self.create_symbol_expr(var_name, ty, *offset)?
            }
            Expression::BinOp(kind, param1, param2, ty) => {
                match kind {
                    BinOpKind::Add => self.create_addition(param1, param2, ty)?,
                    BinOpKind::Sub => self.create_subtraction(param1, param2, ty)?,
                    BinOpKind::Mul => self.create_multiplication(param1, param2, ty)?,
                    BinOpKind::Div => self.create_division(param1, param2, ty)?,
                    BinOpKind::Eq => self.create_compare(CompKind::Equal, param1, param2, ty)?,
                    BinOpKind::Neq => self.create_compare(CompKind::NotEqual, param1, param2, ty)?,
                }
            }
            Expression::UnOp(kind, param, ty) => {
                match kind {
                    UnOpKind::Neg => self.create_negation(param, ty)?,
                }
            }
            Expression::Array(exprs) => {
                self.create_record(exprs)?
            }
            Expression::New(_data, exprs) => {
                self.create_record(exprs)?
            }
            Expression::Fn(_) => vec![], //ignore, handled before function codegen
            Expression::Impl(_, _) => vec![], //ignore, handled before function codegen
            Expression::Data(_) => vec![], //ignore, handled before function codegen
        };
        Ok(value)
    }

    fn create_fn_call(
        &mut self,
        signature: &Signature,
        params: &[Expression],
    ) -> Result<Vec<Value>> {
        let callee = if let Some(f) = self.functions.get(signature.name()) {
            *f
        } else {
            let sig = {
                let mut sig = self.module.make_signature();
                for param_type in signature.params() {
                    let ty = GenType::from_type(param_type, self.module)?;
                    ty.add_to_params(&mut sig.params);
                }
                let ty = GenType::from_type(signature.returns(), self.module)?;
                ty.add_to_params(&mut sig.returns);
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
            let param = self.create_expression(param)?;
            param_values.extend(param);
        }

        let inst = self.builder.ins().call(callee, &param_values);
        let return_values = self.builder.inst_results(inst).to_vec();
        Ok(return_values)
    }

    fn create_return(
        &mut self,
        expr: &Expression
    ) -> Result<Vec<Value>> {
        let values = self.create_expression(expr)?;
        self.builder.ins().return_(&values);
        Ok(values)
    }

    fn create_parameter(
        &mut self,
        name: String,
        param_range: Range<usize>,
        ty: &GenType,
        block: Block,
    ) -> Result<Vec<Value>> {
        let values = self.builder.block_params(block)[param_range].to_vec();
        self.create_variable(name, values, ty)
    }
    
    fn create_local_variable(
        &mut self,
        name: String,
        expr: &Expression,
        ty: &GenType
    ) -> Result<Vec<Value>> {
        let value = self.create_expression(expr)?;
        self.create_variable(name, value, ty)
    }

    fn create_variable(
        &mut self,
        name: String,
        values: Vec<Value>,
        ty: &GenType
    ) -> Result<Vec<Value>> {
        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.size()));
        for (i, &value) in values.iter().enumerate() {
            self.stack_store(value, stack_slot, 8*i as i32);
        }
        self.variables.insert(name, stack_slot);
        Ok(values)
    }

    fn create_variable_mutation(
        &mut self,
        name: &str,
        expr: &Expression
    ) -> Result<Vec<Value>> {
        let values = self.create_expression(expr)?;
        let ss = *self.variables.get(name).unwrap();
        for (i, &value) in values.iter().enumerate() {
            self.stack_store(value, ss, 8*i as i32);
        }
        Ok(values)
    }

    fn create_group(&mut self, body: &[Expression]) -> Result<Vec<Value>> { 
        let group_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        self.builder.ins().jump(group_block, &[]);

        //group block
        self.builder.switch_to_block(group_block);

        for statement in body.iter().take(body.len()-1) {
            self.create_expression(statement)?;
        }

        let result = match body.last() {
            Some(last) => self.create_expression(last)?,
            None => vec![],
        };
        
        self.builder.ins().jump(after_block, &[]);
        
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        self.builder.seal_block(group_block);
        Ok(result)
    }

    fn create_if(&mut self, condition: &Expression, if_body: &[Expression], else_body: &Option<Vec<Expression>>) -> Result<Vec<Value>> {
        let has_else = else_body.is_some();
        let if_block = self.builder.create_block();
        let after_block = self.builder.create_block();
        let branch_block = if has_else { 
            self.builder.create_block()
        } else { after_block };

        let cond = self.create_expression(condition)?[0];
        self.builder.ins().brz(cond, branch_block, &[]);
        self.builder.ins().jump(if_block, &[]);

        //if block
        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);

        for statement in if_body {
            self.create_expression(statement)?;
        }
        
        self.builder.ins().jump(after_block, &[]);

        //else block
        if let Some(else_body) = else_body {
            self.builder.switch_to_block(branch_block);
            self.builder.seal_block(branch_block);
    
            for statement in else_body {
                self.create_expression(statement)?;
            }
            
            self.builder.ins().jump(after_block, &[]);
        }
        
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        Ok(vec![])
    }

    fn create_while(&mut self, condition: &Expression, while_body: &[Expression]) -> Result<Vec<Value>> {  
        let check_block = self.builder.create_block();
        let while_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        self.builder.ins().jump(check_block, &[]);

        //check block
        self.builder.switch_to_block(check_block);

        let cond = self.create_expression(condition)?[0];
        self.builder.ins().brz(cond, after_block, &[]);
        self.builder.ins().jump(while_block, &[]);

        //while block
        self.builder.switch_to_block(while_block);

        for statement in while_body {
            self.create_expression(statement)?;
        }
        
        self.builder.ins().jump(check_block, &[]);
        
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        self.builder.seal_block(check_block);
        self.builder.seal_block(while_block);
        Ok(vec![])
    }

    fn create_index(&mut self, collection: &Expression, index: &Expression, ty: &GenType, coll_length: u32) -> Result<Vec<Value>> {
        let index = self.create_expression(index)?[0];

        let ss = match collection {
            Expression::Symbol(name, _) => *self.variables.get(name).unwrap(),
            _ => {
                let values = self.create_expression(collection)?;
                let ss = self.builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.size()*coll_length));
                for (i, &value) in values.iter().enumerate() {
                    self.stack_store(value, ss, 8*i as i32);
                }
                ss
            }
        };

        Ok(self.create_indexed(ss, ty, index))
    }

    fn create_indexed(&mut self, ss: StackSlot, ty: &GenType, index: Value) -> Vec<Value> {
        let addr = self.builder.ins().stack_addr(I64, ss, 0);
        let index = self.builder.ins().imul_imm(index, ty.size() as i64);
        let new_addr = self.builder.ins().iadd(addr, index);

        ty.types().into_iter().zip(ty.offsets())
            .map(|(ty, offset)|{
                self.builder.ins().load(*ty, MemFlags::new(), new_addr, offset)
            })
            .collect()
    }

    fn create_for(&mut self, iter: &Expression, var_name: String, var_type: &GenType, for_body: &[Expression]) -> Result<Vec<Value>> {  
        if let Expression::Iter(coll, len) = iter {
            if *len == 0 { return Ok(vec![]); }

            let ss = match &**coll {
                Expression::Symbol(name, _) => *self.variables.get(name).unwrap(),
                Expression::Array(_) => {
                    let ss = self.builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, var_type.size()*len));
                    let values = self.create_expression(coll)?;
                    for (i, &value) in values.iter().enumerate() {
                        self.stack_store(value, ss, 8*i as i32);
                    }
                    ss
                },
                _ => return Err(Error::codegen("Expected array in iter of for loop".to_string(), 0)),
            };

            let init_block = self.builder.create_block();
            let add_block = self.builder.create_block();
            let check_block = self.builder.create_block();
            let for_block = self.builder.create_block();
            let after_block = self.builder.create_block();

            self.builder.ins().jump(init_block, &[]);

            //init block
            self.builder.switch_to_block(init_block);

            let iter_var = Variable::new(self.var_counter.next());
            self.builder.declare_var(iter_var, I64);
            let init_value = self.builder.ins().iconst(I64, 0);
            self.builder.def_var(iter_var, init_value);
            self.builder.ins().jump(check_block, &[]);

            //add block
            self.builder.switch_to_block(add_block);

            let arg1 = self.builder.use_var(iter_var);
            let arg2 = self.builder.ins().iconst(I64, 1);
            let new_value = self.builder.ins().iadd(arg1, arg2);
            self.builder.def_var(iter_var, new_value);

            self.builder.ins().jump(check_block, &[]);

            //check block
            self.builder.switch_to_block(check_block);

            let arg1 = self.builder.use_var(iter_var);
            let arg2 = self.builder.ins().iconst(I64, *len as i64);
            let cond = self.builder.ins().icmp(CompKind::Equal.to_intcc(), arg1, arg2);
            self.builder.ins().brnz(cond, after_block, &[]);

            self.builder.ins().jump(for_block, &[]);

            //for block
            self.builder.switch_to_block(for_block);

            let iter_var = self.builder.use_var(iter_var);
            let indexed_values = self.create_indexed(ss, var_type, iter_var);

            match self.variables.get(&var_name) {
                Some(ss) => {
                    let ss = ss.clone();
                    for (i, &value) in indexed_values.iter().enumerate() {
                        self.stack_store(value, ss, 8*i as i32);
                    }
                },
                None => { self.create_variable(var_name.clone(), indexed_values, var_type)?; },
            };

            for statement in for_body {
                self.create_expression(statement)?;
            }
            
            self.builder.ins().jump(add_block, &[]);
                
            //after block
            self.builder.switch_to_block(after_block);
            self.builder.seal_block(init_block);
            self.builder.seal_block(add_block);
            self.builder.seal_block(check_block);
            self.builder.seal_block(for_block);
            self.builder.seal_block(after_block);
            Ok(vec![])
        } else {
            Err(Error::codegen("Expected iter".to_string(), 0))
        }
    }

    fn stack_store(&mut self, value: Value, stack_slot: StackSlot, offset: i32) {
        if self.builder.ins().data_flow_graph().value_type(value).is_bool() {
            let value = self.builder.ins().bint(I64, value);
            self.builder.ins().stack_store(value, stack_slot, offset);
        } else {
            self.builder.ins().stack_store(value, stack_slot, offset);
        }
    }

    fn stack_load(&mut self, ty: ir::Type, stack_slot: StackSlot, offset: i32) -> Value {
        if ty.is_bool() {
            let value = self.builder.ins().stack_load(I64, stack_slot, offset);
            self.builder.ins().icmp_imm(CompKind::NotEqual.to_intcc(), value, 0)
        } else {
            self.builder.ins().stack_load(ty, stack_slot, offset)
        }
    }

    fn create_pointer_to_stack_slot(&mut self, values: &[Value]) -> Result<Vec<Value>> {
        let slot_data = StackSlotData::new(StackSlotKind::ExplicitSlot, 8*values.len() as u32);
        let stack_slot = self.builder.create_sized_stack_slot(slot_data);
        for (i, &value) in values.iter().enumerate() {
            self.stack_store(value, stack_slot, 8*i as i32);
        }
        Ok(vec![self.builder.ins().stack_addr(I64, stack_slot, 0)])
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

    fn create_addition(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().iadd(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().fadd(v1, v2)])
            },
            Type::String => self.create_fn_call(&Signature::new("strcat",vec![Type::Int,Type::Int],Type::Int), &vec![param1.clone(), param2.clone()]),
            _ => Err(Error::codegen("Addition for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_subtraction(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().isub(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().fsub(v1, v2)])
            },
            _ => Err(Error::codegen("Subtration for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_multiplication(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().imul(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().fmul(v1, v2)])
            },
            _ => Err(Error::codegen("Multiplication for this type is not supported".to_string(), 0))
        }
    }
    
    fn create_division(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().sdiv(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().fdiv(v1, v2)])
            },
            _ => Err(Error::codegen("Division for this type is not supported".to_string(), 0))
        }
    }

    fn create_negation(&mut self, param: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v = self.create_expression(param)?[0];
                Ok(vec![self.builder.ins().ineg(v)])
            },
            Type::Float => {
                let v = self.create_expression(param)?[0];
                Ok(vec![self.builder.ins().fneg(v)])
            },
            Type::Bool => {
                let v = self.create_expression(param)?[0];
                Ok(vec![self.builder.ins().bnot(v)])
            },
            _ => Err(Error::codegen("Division for this type is not supported".to_string(), 0))
        }
    }

    fn create_compare(&mut self, kind: CompKind, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int | Type::Enum(_) => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().icmp(kind.to_intcc(), v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression(param1)?[0];
                let v2 = self.create_expression(param2)?[0];
                Ok(vec![self.builder.ins().fcmp(kind.to_floatcc(), v1, v2)])
            },
            _ => Err(Error::codegen("Compare for this type is not supported".to_string(), 0))
        }
    }

    fn create_record(&mut self, exprs: &Vec<Expression>) -> Result<Vec<Value>> {
        exprs.iter()
            .map(|e| self.create_expression(e))
            .flatten_ok()
            .collect()
    }

    fn create_symbol_expr(&mut self, var_name: &String, ty: &Type, field_offset: i32) -> Result<Vec<Value>> {
        assert!(field_offset >= 0);
        let ss = *self.variables.get(var_name).unwrap();
        let ty = GenType::from_type(ty, self.module)?;
        let values = ty.types().into_iter().zip(ty.offsets()).map(|(ty, offset)|{
            self.stack_load(*ty, ss, offset + field_offset)
        }).collect();
        Ok(values)
    }
    
}