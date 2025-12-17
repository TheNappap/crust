
use std::collections::HashMap;

use std::ops::{RangeFrom, Range};

use cranelift_codegen::entity::EntityRef;
use cranelift_codegen::ir::types::{I8, I64};
use cranelift_codegen::ir::{Block, FuncRef, Function, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, UserFuncName, Value};

use cranelift_codegen::verifier::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable, Switch};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
use itertools::Itertools;

use crate::utils::{Result, ThrowablePosition, try_option};
use crate::lexer::Literal;
use crate::parser::{BinOpKind, Expression, ExpressionKind, Symbol, Fn, OrderedMap, Pattern, Signature, TransformKind, Type, UnOpKind};

use super::comp_kind::CompKind;
use super::gen_type::GenType;

pub fn create_fn<'codegen>(
    fun_ctx: &'codegen mut FunctionBuilderContext,
    data_ctx: &'codegen mut DataDescription,
    module: &'codegen mut ObjectModule,
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

    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    let builder = FunctionBuilder::new(&mut func, fun_ctx);

    let ctx = FunctionCodegenContext {
        data_ctx,
        module,
        path: path.to_string() + fun.signature().name(),
        variables: HashMap::new(),
        functions: HashMap::new(),
        str_counter: Counter::new(),
        var_counter: Counter::new(),
    };

    let fun_codegen = FunctionCodegen::new(ctx, builder);
    fun_codegen.create_fn(fun)?;

    //println!("{}", func.display());
    verify_function(&func, module.isa())?;

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

struct FunctionCodegenContext<'codegen> {
    data_ctx: &'codegen mut DataDescription,
    module: &'codegen mut ObjectModule,
    path: String,
    variables: HashMap<String, StackSlot>,
    functions: HashMap<String, FuncRef>,
    str_counter: Counter,
    var_counter: Counter,
}

struct FunctionCodegen<'codegen> {
    ctx: FunctionCodegenContext<'codegen>,
    builder: FunctionBuilder<'codegen>,
}

impl<'codegen> FunctionCodegen<'codegen> {
    fn new(ctx: FunctionCodegenContext<'codegen>, builder: FunctionBuilder<'codegen>) -> Self {
        FunctionCodegen {ctx, builder}
    }

    fn create_fn(mut self, fun: &Fn) -> Result<()> {
        let block = self.builder.create_block();
        self.builder.append_block_params_for_function_params(block);
        
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);

        let mut offset = 0;
        for (name, ty) in fun.params() {
            let new_offset = offset + GenType::from_type(ty, self.ctx.module)?.offsets().len();
            let symbol = Symbol{name: name.clone(), ty: ty.clone() };
            self.create_parameter(&symbol, offset..new_offset, block)?;
            offset = new_offset;
        }

        for statement in fun.body() {
            self.create_expression(statement)?;
        }

        self.builder.finalize();
        Ok(())
    }

    fn create_expression_list(&mut self, expressions: &Vec<Expression>) -> Result<Option<Vec<Value>>> {
        let mut list = vec![];
        for expression in expressions {
            match self.create_expression(expression)? {
                None => return Ok(None),
                Some(value) => list.extend(value),
            }
        }
        Ok(Some(list))
    }

    fn create_expression_value(&mut self, expression: &Expression) -> Result<Vec<Value>> {
        if let Some(values) = self.create_expression(expression)? {
            Ok(values)
        } else {
            expression.span.codegen("Expected value for expression, but found never type".into())
        }
    }

    fn create_expression(&mut self, expression: &Expression) -> Result<Option<Vec<Value>>> {
        let value = match &expression.kind {
            ExpressionKind::Void => vec![],
            ExpressionKind::Call(signature, params) => {
                let param_values = try_option!(self.create_expression_list(params));
                self.create_fn_call(signature, &param_values)?
            }
            ExpressionKind::Return(expr) => {
                try_option!(self.create_return(expr))
            }
            ExpressionKind::Let(symbol, expr) => {
                try_option!(self.create_local_variable(symbol, expr))
            }
            ExpressionKind::Mut(symbol, field, expr) => {
                try_option!(self.create_variable_mutation(&symbol.name, field.clone(), expr))
            }
            ExpressionKind::If(condition, if_body, else_body, ty) => {
                try_option!(self.create_if(condition, if_body, else_body, &GenType::from_type(ty, self.ctx.module)?))
            }
            ExpressionKind::While(condition, while_body) => {
                try_option!(self.create_while(condition, while_body))
            }
            ExpressionKind::Fold(iter, var_symbol, accumulator, for_body) => {
                try_option!(self.create_fold(iter, var_symbol, for_body, accumulator))
            }
            ExpressionKind::Iter(iter, _, _) => {
                try_option!(self.create_expression(iter))
            }
            ExpressionKind::Index(collection, index, ty, coll_length) => {
                self.create_index(collection, index, &GenType::from_type(ty, self.ctx.module)?, *coll_length)?
            }
            ExpressionKind::Group(body, ty) => {
                try_option!(self.create_group(body, &GenType::from_type(ty, self.ctx.module)?))
            }
            ExpressionKind::Literal(literal) => {
                vec![self.create_literal(literal)?]
            },
            ExpressionKind::AddrOf(expressions) => {
                let values = try_option!(self.create_expression_list(expressions));
                self.create_pointer_to_stack_slot(&values)?
            }
            ExpressionKind::Symbol(symbol) => {
                self.create_symbol_expr(symbol, 0)?
            }
            ExpressionKind::Field(expr, field_symbol, offset) => {
                let parent_symbol = if let ExpressionKind::Symbol(symbol) = &expr.kind {
                    Symbol{name: symbol.name.clone(), ty: field_symbol.ty.clone()}
                } else {
                    let name = "__field_temp_".to_string() + &self.ctx.var_counter.next().to_string();
                    let symbol = Symbol{name, ty: field_symbol.ty.clone()};
                    self.create_local_variable(&symbol, expr)?;
                    symbol
                };
                self.create_symbol_expr(&parent_symbol, *offset)?
            }
            ExpressionKind::BinOp(kind, param1, param2, ty) => {
                match kind {
                    BinOpKind::Add => self.create_addition(param1, param2, ty)?,
                    BinOpKind::Sub => self.create_subtraction(param1, param2, ty)?,
                    BinOpKind::Mul => self.create_multiplication(param1, param2, ty)?,
                    BinOpKind::Div => self.create_division(param1, param2, ty)?,
                    BinOpKind::Eq => self.create_compare(CompKind::Equal, param1, param2, ty)?,
                    BinOpKind::Neq => self.create_compare(CompKind::NotEqual, param1, param2, ty)?,
                }
            }
            ExpressionKind::UnOp(kind, param, ty) => {
                match kind {
                    UnOpKind::Neg => self.create_negation(param, ty)?,
                }
            }
            ExpressionKind::Array(exprs) => {
                self.create_record(exprs)?
            }
            ExpressionKind::Range(start, end) => {
                let start_value = self.builder.ins().iconst(I64, *start);
                let end_value = self.builder.ins().iconst(I64, *end);
                vec![start_value, end_value]
            }
            ExpressionKind::New(ty, exprs) => {
                match ty {
                    Type::Struct(_, _) => self.create_record(exprs)?,
                    Type::Enum(_, variants) => {
                        let ExpressionKind::Data(data) = &exprs[0].kind else {
                            return expression.span.codegen("Enum variant parsing failed".to_string());
                        };
                        let Some(variant) = variants.get(data.name()) else {
                            return expression.span.codegen("Enum variant not found".to_string());
                        };
                        vec![self.builder.ins().iconst(I64, *variant as i64)]
                    }
                    _ => unreachable!(),
                }
            }
            ExpressionKind::Match(expr, ty, cases) => {
                try_option!(self.create_match(expr, ty, cases))
            },
            ExpressionKind::Signature(_) => vec![], //ignore, handled before function codegen
            ExpressionKind::Fn(_) => vec![], //ignore, handled before function codegen
            ExpressionKind::Trait(_) => vec![], //ignore, handled before function codegen
            ExpressionKind::Impl(..) => vec![], //ignore, handled before function codegen
            ExpressionKind::Data(_) => vec![], //ignore, handled before function codegen
            ExpressionKind::Case(..) => unreachable!(),
        };
        Ok(Some(value))
    }

    fn create_fn_call(
        &mut self,
        signature: &Signature,
        param_values: &[Value],
    ) -> Result<Vec<Value>> {
        let callee = if let Some(f) = self.ctx.functions.get(signature.name()) {
            *f
        } else {
            let sig = {
                let mut sig = self.ctx.module.make_signature();
                for param_type in signature.params() {
                    let ty = GenType::from_type(param_type, self.ctx.module)?;
                    ty.add_to_params(&mut sig.params);
                }
                let ty = GenType::from_type(signature.returns(), self.ctx.module)?;
                ty.add_to_params(&mut sig.returns);
                sig
            };

            let id = self.ctx
                .module
                .declare_function(signature.name(), Linkage::Import, &sig)?;
        
            let callee = self.ctx.module.declare_func_in_func(id, self.builder.func);
            self.ctx.functions.insert(signature.name().to_string(), callee);
            callee
        };

        let inst = self.builder.ins().call(callee, &param_values);
        let return_values = self.builder.inst_results(inst).to_vec();
        Ok(return_values)
    }

    fn create_return(
        &mut self,
        expr: &Expression,
    ) -> Result<Option<Vec<Value>>> {
        if let Some(values) = self.create_expression(expr)? {
            self.builder.ins().return_(&values);
        }
        Ok(None)
    }

    fn create_parameter(
        &mut self,
        symbol: &Symbol,
        param_range: Range<usize>,
        block: Block,
    ) -> Result<Vec<Value>> {
        let values = self.builder.block_params(block)[param_range].to_vec();
        self.create_variable(symbol, values)
    }
    
    fn create_local_variable(
        &mut self,
        symbol: &Symbol,
        expr: &Expression,
    ) -> Result<Option<Vec<Value>>> {
        if let Some(value) = self.create_expression(expr)? {
            return Ok(Some(self.create_variable(symbol, value)?));
        }
        Ok(None)
    }

    fn create_variable(
        &mut self,
        symbol: &Symbol,
        values: Vec<Value>,
    ) -> Result<Vec<Value>> {
        let ty = &GenType::from_type(&symbol.ty, self.ctx.module)?;
        let stack_slot = self.create_stack_slot(&values, ty.size());
        self.ctx.variables.insert(symbol.name.clone(), stack_slot);
        Ok(values)
    }

    fn create_variable_mutation(
        &mut self,
        name: &str,
        field: Option<(Symbol, i32)>,
        expr: &Expression
    ) -> Result<Option<Vec<Value>>> {
        if let Some(values) = self.create_expression(expr)? {
            let ss = *self.ctx.variables.get(name).unwrap();
            let field_offset = if let Some((_,offset)) = field {
                offset
            } else { 0 };
            self.stack_store(&values, ss, field_offset);
            return Ok(Some(values));
        }
        Ok(None)
    }

    fn create_group(&mut self, body: &[Expression], ty: &GenType) -> Result<Option<Vec<Value>>> { 
        let group_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        self.builder.ins().jump(group_block, &[]);

        //group block
        self.builder.switch_to_block(group_block);

        let stack_slot = self.create_stack_slot(&[], ty.size());

        let mut never = false;
        for statement in body {
            if let Some(values) = self.create_expression(statement)? {
                if statement.forward {
                    self.stack_store(&values, stack_slot, 0);
                }
            } else {
                never = true;
                break;
            };
        }
        
        if !never {
            self.builder.ins().jump(after_block, &[]);
        }
         
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        self.builder.seal_block(group_block);

        if never {
            Ok(None)
        } else {
            Ok(Some(self.stack_load(ty, stack_slot, 0)))
        }
    }

    fn create_if(&mut self, condition: &Expression, if_body: &[Expression], else_body: &Option<Vec<Expression>>, ty: &GenType) -> Result<Option<Vec<Value>>> {
        let if_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        let stack_slot = self.create_stack_slot(&[], ty.size());

        let cond = self.create_expression_value(condition)?[0];
        self.builder.ins().brif(cond, if_block, &[],
                                        else_block, &[]);

        //if block
        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);
        let mut never_if = false;
        for statement in if_body {
            if let Some(values) = self.create_expression(statement)? {
                if statement.forward {
                    self.stack_store(&values, stack_slot, 0);
                }
            } else {
                never_if = true;
                break;
            }
        }
        if !never_if {
            self.builder.ins().jump(after_block, &[]);
        }

        //else block
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let mut never_else = false;
        if let Some(else_body) = else_body {
            for statement in else_body {
                if let Some(values) = self.create_expression(statement)? {
                    if statement.forward {
                        self.stack_store(&values, stack_slot, 0);
                    }
                } else {
                    never_else = true;
                    break;
                }
            }
        }
        if !never_else {
            self.builder.ins().jump(after_block, &[]);
        }
        
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        
        if never_if && never_else {
            Ok(None)
        } else {
            Ok(Some(self.stack_load(ty, stack_slot, 0)))
        }
    }

    fn create_while(&mut self, condition: &Expression, while_body: &[Expression]) -> Result<Option<Vec<Value>>> {  
        let check_block = self.builder.create_block();
        let while_block = self.builder.create_block();
        let after_block = self.builder.create_block();

        self.builder.ins().jump(check_block, &[]);

        //check block
        self.builder.switch_to_block(check_block);

        let cond = self.create_expression_value(condition)?[0];
        self.builder.ins().brif(cond, while_block, &[],
                                        after_block, &[]);

        //while block
        self.builder.switch_to_block(while_block);

        for statement in while_body {
            if let None = self.create_expression(statement)? {
                break;
            }
        }
        
        self.builder.ins().jump(check_block, &[]);
        
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(after_block);
        self.builder.seal_block(check_block);
        self.builder.seal_block(while_block);
        Ok(None)
    }

    fn create_index(&mut self, collection: &Expression, index: &Expression, ty: &GenType, coll_length: u32) -> Result<Vec<Value>> {
        let index = self.create_expression_value(index)?[0];

        let ss = match &collection.kind {
            ExpressionKind::Symbol(symbol) => *self.ctx.variables.get(&symbol.name).unwrap(),
            _ => {
                let values = self.create_expression_value(collection)?;
                self.create_stack_slot(&values, ty.size()*coll_length)
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

    fn create_fold(&mut self, iter: &Expression, var_symbol: &Symbol, for_body: &[Expression], accumulator: &Option<(Box<Expression>, Symbol)>) -> Result<Option<Vec<Value>>> {
        let ExpressionKind::Iter(coll, iter_transforms, len) = &iter.kind else {
            return iter.span.codegen("Expected iter".to_string())
        };

        if *len == 0 { return Ok(Some(vec![])); }

        let var_type = &GenType::from_type(&var_symbol.ty, self.ctx.module)?;

        let ss = match &coll.kind {
            ExpressionKind::Symbol(symbol) => *self.ctx.variables.get(&symbol.name).unwrap(),
            ExpressionKind::Array(_) | ExpressionKind::Range(_, _) => {
                let values = self.create_expression_value(coll)?;
                self.create_stack_slot(&values, var_type.size()**len)
            },
            _ => return coll.span.codegen("Expected array or range in iter of loop".to_string()),
        };

        let init_block = self.builder.create_block();
        let add_block = self.builder.create_block();
        let check_block = self.builder.create_block();
        let for_block = self.builder.create_block();
        let after_block = self.builder.create_block();
        self.builder.ins().jump(init_block, &[]);

        //init block
        self.builder.switch_to_block(init_block);
        let iter_var = Variable::new(self.ctx.var_counter.next());
        self.builder.declare_var(iter_var, I64);
        let init_value = self.builder.ins().iconst(I64, 0);
        self.builder.def_var(iter_var, init_value);

        if let Some((init, acc_symbol)) = accumulator {
            self.create_local_variable(acc_symbol, init)?;
        }
        
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
        self.builder.ins().brif(cond, after_block, &[],
                                        for_block, &[]);

        //for block
        self.builder.switch_to_block(for_block);
        let iter_var = self.builder.use_var(iter_var);
        
        let indexed_values = match &coll.kind {
            ExpressionKind::Range(start, _) => {
                let start_value = self.builder.ins().iconst(I64, *start);
                let new_value = self.builder.ins().iadd(iter_var, start_value);
                vec![new_value]
            },
            _ => self.create_indexed(ss, var_type, iter_var),
        };
        let transformed_values = iter_transforms.into_iter()
            .fold(Ok(indexed_values), |values, transform|{
                match transform.kind {
                    TransformKind::Map => self.create_fn_call(transform.fun.signature(), &values?),
                    TransformKind::Filter => {
                        let if_block = self.builder.create_block();
                        let after_block = self.builder.create_block();
                        let cond = self.create_fn_call(transform.fun.signature(), &values.clone()?)?[0];
                        self.builder.ins().brif(cond, if_block, &[], after_block, &[]);
                        //if block
                        self.builder.switch_to_block(if_block);
                        self.builder.seal_block(if_block);
                        self.builder.ins().jump(add_block, &[]);
                        //after block
                        self.builder.switch_to_block(after_block);
                        self.builder.seal_block(after_block);
                        values
                    },
                }
            })?;
        self.create_variable(var_symbol, transformed_values)?;

        let mut never = false;
        for statement in for_body {
            let value = if let Some((_, acc_symbol)) = accumulator && statement.forward {
                self.create_variable_mutation(&acc_symbol.name, None, &statement)?
            } else {
                self.create_expression(statement)?
            };
            if value.is_none() {
                never = true;
                break;
            }
        }
        
        self.builder.ins().jump(add_block, &[]);
            
        //after block
        self.builder.switch_to_block(after_block);
        self.builder.seal_block(init_block);
        self.builder.seal_block(add_block);
        self.builder.seal_block(check_block);
        self.builder.seal_block(for_block);
        self.builder.seal_block(after_block);

        if never {
            Ok(None)
        } else if let Some((_, acc_symbol)) = accumulator {
            Some(self.create_symbol_expr(acc_symbol, 0)).transpose()
        } else {
            Ok(Some(vec![]))
        }
    }

    fn create_pointer_to_stack_slot(&mut self, values: &[Value]) -> Result<Vec<Value>> {
        let stack_slot = self.create_stack_slot(values, 8*values.len() as u32);
        Ok(vec![self.builder.ins().stack_addr(I64, stack_slot, 0)])
    }

    fn create_literal(&mut self, literal: &Literal) -> Result<Value> {
        let literal = match literal {
            Literal::Int(i) => self.builder.ins().iconst(I64, *i),
            Literal::Float(f) => self.builder.ins().f64const(*f),
            Literal::Bool(b) => self.builder.ins().iconst(I8, *b as i64),
            Literal::String(s) => self.create_literal_string(s.clone())?
        };
        Ok(literal)
    }

    fn create_literal_string(&mut self, mut str: String) -> Result<Value> {
        str.push('\0');
        let name = self.ctx.path.clone() + "_string_" + &self.ctx.str_counter.next().to_string();
        let contents = str.as_bytes().to_vec().into_boxed_slice();

        self.create_data(&name, contents)
    }
    
    fn create_data(&mut self, name: &str, contents: Box<[u8]>) -> Result<Value> {
        self.ctx.data_ctx.define(contents);
        let data = self.ctx
            .module
            .declare_data(name, Linkage::Local, true, false)?;
        self.ctx.module.define_data(data, self.ctx.data_ctx)?;
        self.ctx.data_ctx.clear();

        
        let global_value = self.ctx.module.declare_data_in_func(data, self.builder.func);
        let pointer = self.ctx.module.target_config().pointer_type();
        let value = self.builder.ins().symbol_value(pointer, global_value);
        Ok(value)
    }

    fn create_addition(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().iadd(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().fadd(v1, v2)])
            },
            Type::String => {
                let mut param_values= self.create_expression_value(param1)?;
                param_values.extend(self.create_expression_value(param2)?);
                self.create_fn_call(&Signature::new(None, "strcat",vec![Type::Int,Type::Int],Type::Int), &param_values)
            }
            _ => param1.span.codegen("Addition for this type is not supported".to_string())
        }
    }
    
    fn create_subtraction(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().isub(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().fsub(v1, v2)])
            },
            _ => param1.span.codegen("Subtration for this type is not supported".to_string())
        }
    }
    
    fn create_multiplication(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().imul(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().fmul(v1, v2)])
            },
            _ => param1.span.codegen("Multiplication for this type is not supported".to_string())
        }
    }
    
    fn create_division(&mut self, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().sdiv(v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().fdiv(v1, v2)])
            },
            _ => param1.span.codegen("Division for this type is not supported".to_string())
        }
    }

    fn create_negation(&mut self, param: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int => {
                let v = self.create_expression_value(param)?[0];
                Ok(vec![self.builder.ins().ineg(v)])
            },
            Type::Float => {
                let v = self.create_expression_value(param)?[0];
                Ok(vec![self.builder.ins().fneg(v)])
            },
            Type::Bool => {
                let v = self.create_expression_value(param)?[0];
                let zero = self.builder.ins().iconst(I8, 0);
                Ok(vec![self.builder.ins().icmp(CompKind::Equal.to_intcc(), v, zero)])
            },
            _ => param.span.codegen("Division for this type is not supported".to_string())
        }
    }

    fn create_compare(&mut self, kind: CompKind, param1: &Expression, param2: &Expression, ty: &Type) -> Result<Vec<Value>> {
        match ty {
            Type::Int | Type::Enum(_, _) => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().icmp(kind.to_intcc(), v1, v2)])
            },
            Type::Float => {
                let v1 = self.create_expression_value(param1)?[0];
                let v2 = self.create_expression_value(param2)?[0];
                Ok(vec![self.builder.ins().fcmp(kind.to_floatcc(), v1, v2)])
            },
            _ => param1.span.codegen("Compare for this type is not supported".to_string())
        }
    }

    fn create_record(&mut self, exprs: &Vec<Expression>) -> Result<Vec<Value>> {
        exprs.iter()
            .map(|e| self.create_expression_value(e))
            .flatten_ok()
            .collect()
    }

    fn create_symbol_expr(&mut self, symbol: &Symbol, field_offset: i32) -> Result<Vec<Value>> {
        assert!(field_offset >= 0);
        let ss = *self.ctx.variables.get(&symbol.name).unwrap();
        let ty = GenType::from_type(&symbol.ty, self.ctx.module)?;
        let values = self.stack_load(&ty, ss, field_offset);
        Ok(values)
    }
    
    fn create_match(&mut self, expr: &Expression, ty: &Type, cases: &OrderedMap<Pattern, Vec<Expression>>) -> Result<Option<Vec<Value>>> {
        let Type::Enum(_, variants) = ty else {
            return expr.span.codegen("Only enum patterns in match".into());
        };

        let values = self.create_expression_value(expr)?;

        let mut switch = Switch::new();
        let (cases, default) = cases.iter()
            .partition::<Vec<_>, _>(|(pattern, _)| {
                match pattern {
                    Pattern::Ident(_) => false,
                    Pattern::EnumVariant(_, _) => true,
                }
            });
        assert!(default.len() < 2);

        let blocks = cases.into_iter().map(|(pattern, exprs)| {
                let block = self.builder.create_block();
                match pattern {
                    Pattern::Ident(_) => unreachable!(),
                    Pattern::EnumVariant(_, name) => switch.set_entry(variants[name] as u128, block),
                }
                (block, exprs)
            }).collect_vec();

        let (fallback, next_block) = if default.len() == 0 {
            let fallback =  self.builder.create_block();
            (fallback, fallback)
        } else {
            (self.builder.create_block(), self.builder.create_block())
        };
        let default = default.into_iter().map(|(_,exprs)| (fallback, exprs));

        switch.emit(&mut self.builder, values[0], fallback);

        let mut never = false;
        for (block, exprs) in blocks.into_iter().chain(default) {
            self.builder.switch_to_block(block);
            let mut never_block = false;
            for expr in exprs {
                if let None = self.create_expression(expr)? {
                    never_block = true;
                    break;
                }
            }
            never = never && never_block;
            self.builder.ins().jump(next_block, &[]);
            self.builder.seal_block(block);
        }
        
        self.builder.switch_to_block(next_block);
        self.builder.seal_block(next_block);
        if never {
            return Ok(None)
        } else {
            Ok(Some(vec![]))
        }
    }

    fn create_stack_slot(&mut self, values: &[Value], size: u32) -> StackSlot {
        let data = StackSlotData::new(StackSlotKind::ExplicitSlot, size, 0);
        let stack_slot = self.builder.create_sized_stack_slot(data);
        self.stack_store(values, stack_slot, 0);
        return stack_slot;
    }
    
    fn stack_store(&mut self, values: &[Value], stack_slot: StackSlot, offset: i32) {
        for (i, &value) in values.iter().enumerate() {
            self.builder.ins().stack_store(value, stack_slot, offset + 8*i as i32);
        }
    }
    
    fn stack_load(&mut self, ty: &GenType, stack_slot: StackSlot, offset: i32) -> Vec<Value> {
        ty.types().into_iter().zip(ty.offsets()).map(|(ty, value_offset)|{
            self.builder.ins().stack_load(*ty, stack_slot, offset + value_offset)
        }).collect()
    }
}