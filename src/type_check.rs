
use std::{collections::{hash_map::Entry, HashMap}};

use itertools::Itertools;

use crate::{parser::{SyntaxTree, Type, Fn, Expression, ExpressionKind, Signature, BinOpKind}, error::{Result, Error, ThrowablePosition, ErrorKind}, lexer::{Literal, Span, Position}};


pub fn type_check(syntax_tree: &mut SyntaxTree) -> Result<()> {
    let mut data_map = HashMap::new();
    let _: () = syntax_tree.data_types()
        .map(|(data, span)| match data_map.entry(data.name().to_owned()) {
            Entry::Occupied(_) => return Err(span.error(ErrorKind::Type, "Data structure already defined".to_string())),
            Entry::Vacant(v) => { v.insert((data.to_owned(), span.to_owned())); Ok(()) },
        })
        .try_collect()?;

    let mut functions: HashMap<_,_> = syntax_tree.imports().cloned().map(|sig| (sig.name().to_owned(), sig) ).collect();
    for fun in syntax_tree.fns_impls() {
        for expr in fun.body() {
            if let ExpressionKind::Fn(f) = &expr.kind {
                functions.insert(f.signature().name().to_string(), f.signature().clone());
            }
        }
        functions.insert(fun.signature().name().to_string().clone(), fun.signature().clone()); 
    }
    functions.values_mut().try_for_each(|sig| sig.params_mut().try_for_each(|ty| data_map.check_named_type(ty)))?;

    let mut type_checker = TypeCheck::new(&functions, &data_map);
    for fun in syntax_tree.fns_impls_mut() {
        type_checker.check_fun(fun)?
    }
    syntax_tree.add_hidden_fns(type_checker.hidden_fns());
    Ok(())
}

trait DataTypeCheck {
    fn check_type_name(&self, name: &str) -> Result<Type>;

    fn check_named_type(&self, ty: &mut Type) -> Result<()> {
        if let Type::Named(name) = ty {
            *ty = self.check_type_name(name)?.to_owned();
        };
        Ok(())
    } 
}

impl DataTypeCheck for HashMap<String, (Type, Span)> {
    fn check_type_name(&self, name: &str) -> Result<Type> {
        match self.get(name) {
            Some((data, _)) => Ok(data.to_owned()),
            None => return Err(Error::new(ErrorKind::Type, format!("Data structure not found: {}", name), Position::zero()))
        }
    }
}

struct TypeCheck<'f> {
    variables: HashMap<String, Type>,
    functions: &'f HashMap<String, Signature>,
    data_types: &'f HashMap<String, (Type, Span)>,
    hidden_fns: Vec<Fn>,
}

impl<'f> TypeCheck<'f> {
    fn new(functions: &'f HashMap<String, Signature>, data_types: &'f HashMap<String, (Type, Span)>) -> TypeCheck<'f> {
        TypeCheck {
            variables: HashMap::new(),
            functions,
            data_types,
            hidden_fns: vec![],
        }
    }

    fn hidden_fns(self) -> Vec<Fn> {
        self.hidden_fns
    }

    fn check_fun(&mut self, fun: &mut Fn) -> Result<()> {
        if let Some(ty) = fun.signature_mut().self_ty_mut() {
            self.data_types.check_named_type(ty)?;
        }
        for (name, ty) in fun.params_mut() {
            self.data_types.check_named_type(ty)?;
            self.variables.insert(name.clone(), ty.clone());
        }
        self.data_types.check_named_type(fun.signature_mut().returns_mut())?;
        for expr in fun.body_mut() {
            self.check_expression(expr)?;
        }
        if *fun.signature().returns() == Type::Inferred {
            let return_type = fun.body_mut().fold(Ok(Type::Void), |acc, expr| {
                match &mut expr.kind {
                    ExpressionKind::Forward(expr) => self.check_expression(expr),
                    _ => acc,
                }
            })?;
            *fun.signature_mut().returns_mut() = return_type;
        }
        Ok(())
    }

    fn check_group(&mut self, exprs: &mut Vec<Expression>) -> Result<Type> {
        let size = exprs.len();
        for expr in exprs.iter_mut().take(size) {
            self.check_expression(expr)?;
        }
        match exprs.last_mut() {
            None => Ok(Type::Void),
            Some(expr) => self.check_expression(expr),
        }
    }

    fn check_expression(&mut self, expr: &mut Expression) -> Result<Type> {
        let span = expr.span.clone();
        let mut ty = match &mut expr.kind {
            ExpressionKind::Call(signature, params) => {
                if let Some(Type::Inferred) = signature.self_ty() {
                    assert!(params.len() > 0);
                    let ty = self.check_expression(&mut params[0])?;
                    signature.set_self_type(ty.name());
                }
                *signature = self.functions.get(signature.name()).expect("Function not found").clone();

                params.iter_mut()
                      .map(|expr| self.check_expression(expr))
                      .zip(signature.params())
                      .map(|(r,t)| {
                            r.and_then(|ty| {
                                if *t != ty {
                                    return Err(span.error(ErrorKind::Type, format!("Mismatched types for parameter, expected: {:?}, got: {:?}", t, ty).to_string()));
                                }
                                Ok(ty)
                            })
                      })
                      .try_for_each(|x| x.map(|_| ()))?;

                self.data_types.check_named_type(signature.returns_mut())?;
                signature.returns().clone()
            },
            ExpressionKind::Return(expr) => {
                self.check_expression(expr)?
            },
            ExpressionKind::Forward(expr) => {
                self.check_expression(expr)?
            },
            ExpressionKind::Signature(_) => {
                todo!()
            }
            ExpressionKind::Fn(fun) => {
                self.check_fun(fun)?;
                self.hidden_fns.push(fun.clone());
                Type::Inferred
            }
            ExpressionKind::Impl(name, fns, _) => {
                self.data_types.check_type_name(name)?;
                fns.iter_mut().try_for_each(|fun| self.check_fun(fun))?;
                Type::Inferred
            }
            ExpressionKind::Trait(_) => {
                todo!()
            }
            ExpressionKind::Data(_) => {
                Type::Inferred
            }
            ExpressionKind::New(ty, exprs) => {
                *ty = self.data_types.check_type_name(ty.name())?;

                match ty {
                    Type::Struct(_, fields) => {
                        let offsets: Vec<i32> = fields.iter().zip(exprs.into_iter())
                            .map(|((_,(ty, offset)), expr)| -> Result<_> { 
                                let expr_ty =  self.check_expression(expr)?;
                                if *ty != expr_ty {
                                    return Err(expr.span.error(ErrorKind::Type, "Mismatch types for parameter expression".to_string()));
                                }
                                Ok(*offset)
                            })
                            .try_collect()?;
                        for (i, (_, (_, offset))) in  fields.iter_mut().enumerate() {
                            if i == 0 { *offset = 0; }
                            else { *offset = offsets[i] }
                        }
                    }
                    Type::Enum(_, variants) => {
                        assert!(exprs.len() == 1);
                        let ExpressionKind::Data(data) = &exprs[0].kind else {
                            return Err(expr.span.error(ErrorKind::Type, "Enum variant parsing failed".to_string()));
                        };
                        let Some(_) = variants.get(data.name()) else {
                            return Err(expr.span.error(ErrorKind::Type, "Enum variant not found".to_string()));
                        };
                    }
                    _ => return Err(expr.span.error(ErrorKind::Type, "The type of data structure is not well defined".to_string()))
                }
                    
                ty.to_owned()
            }
            ExpressionKind::Field(expr, field_symbol, field_offset) => {
                let ty = self.check_expression(expr)?;

                match ty {
                    Type::Struct(_, map) => {
                        let Some((ty, offset)) = map.get(&field_symbol.name) else { 
                            return Err(expr.span.error(ErrorKind::Type, format!("No field found with this name: {}", &field_symbol.name))); 
                        };

                        field_symbol.ty = ty.to_owned();
                        *field_offset = *offset;
                        ty.to_owned()
                    },
                    _ => return Err(expr.span.error(ErrorKind::Type, format!("No field for type: {:?}", ty)))
                }
            }
            ExpressionKind::Let(symbol, expr) => {
                let ty = self.check_expression(expr)?;
                if symbol.ty != Type::Inferred && symbol.ty != ty {
                    return Err(expr.span.error(ErrorKind::Type, format!("Mismatch types for assignment, expected: {:?}", symbol.ty)));
                }
                self.variables.insert(symbol.name.clone(), ty.clone());
                symbol.ty = ty.clone();
                ty
            },
            ExpressionKind::Mut(symbol, field, expr) => {
                let var_type = self.variables.get(&symbol.name);
                if let Some(var_ty) = var_type.cloned() {
                    let ty = if let Some((field_symbol, field_offset)) = field {
                        if let Type::Struct(_, fields) = var_ty.clone() {
                            if let Some((ty, offset)) = fields.get(&field_symbol.name) {
                                *field_offset = *offset;
                                ty.to_owned()
                            } else {
                                return expr.span.type_("Field name not found".to_string());
                            }
                        } else {
                            return expr.span.type_("Type does not have fields".to_string());
                        }
                    } else {
                        let ty = self.check_expression(expr)?;
                        if var_ty != Type::Inferred && var_ty != ty {
                            return Err(expr.span.error(ErrorKind::Type, format!("Mismatch types for assignment, expected: {:?}, but got {:?}", var_ty, ty)));
                        }
                        ty
                    };
                    ty
                } else {
                    return Err(expr.span.error(ErrorKind::Type, format!("No variable found with this name: {:?}", symbol.name)));
                }
            },
            ExpressionKind::If(condition, if_body, else_body) => {
                let ty = self.check_expression(condition)?;
                if ty != Type::Bool {
                    return Err(expr.span.error(ErrorKind::Type, "Expected a boolean type as condition".to_string()));
                }
                for expr in if_body {
                    self.check_expression(expr)?;
                }
                if let Some(else_body) = else_body {
                    for expr in else_body {
                        self.check_expression(expr)?;
                    }
                }
                Type::Void
            },
            ExpressionKind::While(condition, while_body) => {
                let ty = self.check_expression(condition)?;
                if ty != Type::Bool {
                    return Err(expr.span.error(ErrorKind::Type, "Expected a boolean type as condition".to_string()));
                }
                for expr in while_body {
                    self.check_expression(expr)?;
                }
                Type::Void
            },
            ExpressionKind::Fold(iter, var_symbol, accumulator, for_body) => {
                if let Some(acc) = accumulator {
                    let ty = self.check_expression(&mut acc.0)?;
                    assert!(acc.1.ty == Type::Inferred);
                    acc.1.ty = ty;
                    self.variables.insert(acc.1.name.clone(), acc.1.ty.clone());
                }

                let ty = self.check_expression(iter)?;
                if let Type::Iter(iter_ty) = ty {
                    if let Type::Array(arr_type, _) = *iter_ty {
                        if var_symbol.ty == Type::Inferred {
                            var_symbol.ty = *arr_type.clone();
                        }
                        self.variables.insert(var_symbol.name.clone(), var_symbol.ty.clone());
                        if *arr_type != var_symbol.ty {
                            return Err(expr.span.error(ErrorKind::Type, "Array type and for type do not match".to_string()));
                        }
                        for expr in for_body {
                            self.check_expression(expr)?;
                        }
                        Type::Void
                    } else if let Type::Range(_) = *iter_ty {
                        if var_symbol.ty == Type::Inferred {
                            var_symbol.ty = Type::Int;
                        }
                        self.variables.insert(var_symbol.name.clone(), var_symbol.ty.clone());
                        if Type::Int != var_symbol.ty {
                            return Err(expr.span.error(ErrorKind::Type, "Range type and for type do not match".to_string()));
                        }
                        for expr in for_body {
                            self.check_expression(expr)?;
                        }
                        Type::Void
                    } else {
                        return Err(expr.span.error(ErrorKind::Type, "Expected array or range as iterator".to_string()));
                    }
                } else {
                    return Err(expr.span.error(ErrorKind::Type, "Expected iter as input for loop block".to_string()));
                }
            },
            ExpressionKind::Iter(input_expr, iter_transforms, len) => {
                let ty = self.check_expression(input_expr)?;
                let (iter_len, el_ty) = match &ty {
                    Type::Array(arr_ty, l) => (*l as u32, *arr_ty.clone()),
                    Type::Range(l) => (*l as u32, Type::Int),
                    _ => return Err(expr.span.error(ErrorKind::Type, "Expected array or range as iter input".to_string())),
                };
                *len = iter_len;

                let mut in_type = el_ty;
                for transform in iter_transforms {
                    transform.fun.signature_mut().params_mut().for_each(|param_type| {
                        if *param_type == Type::Inferred {
                            *param_type = in_type.clone();
                        }
                    });

                    self.check_fun(&mut transform.fun)?;
                    self.hidden_fns.push(transform.fun.clone());
                    in_type = transform.fun.signature().returns().clone();
                }
                Type::Iter(Box::new(ty))
            },
            ExpressionKind::Range(start, end) => Type::Range((*end-*start) as u32),
            ExpressionKind::Group(body) => self.check_group(body)?,
            ExpressionKind::Literal(Literal::Int(_)) => Type::Int,
            ExpressionKind::Literal(Literal::Float(_)) => Type::Float,
            ExpressionKind::Literal(Literal::Bool(_)) => Type::Bool,
            ExpressionKind::Literal(Literal::String(_)) => Type::String,
            ExpressionKind::AddrOf(exprs) => {
                for expr in exprs {
                    self.check_expression(expr)?;
                }
                Type::Int
            },
            ExpressionKind::BinOp(kind, param1, param2, op_ty) => {
                let ty = self.check_expression(param1)?;
                if ty != self.check_expression(param2)? {
                    return Err(expr.span.error(ErrorKind::Type, format!("Mismatch types for operands, expected: {:?}", ty)));
                }

                let out_ty = match kind {
                    BinOpKind::Eq | BinOpKind::Neq => Type::Bool,
                    _ => ty.clone()
                };

                if *op_ty != Type::Inferred && *op_ty != out_ty {
                    return Err(span.error(ErrorKind::Type, format!("Mismatch types for binary operation, expected: {:?}", op_ty)));
                }
                *op_ty = ty;
                out_ty
            },
            ExpressionKind::UnOp(_, param, op_ty) => {
                let ty = self.check_expression(param)?;
                if *op_ty != Type::Inferred && *op_ty != ty {
                    return Err(span.error(ErrorKind::Type, format!("Mismatch types for unary operation, expected: {:?}", op_ty)));
                }
                *op_ty = ty.clone();
                ty
            },
            ExpressionKind::Symbol(symbol) => {
                let var_type = self.variables.get(&symbol.name);
                match &symbol.ty {
                    Type::Inferred => match var_type {
                        Some(t) => {
                            symbol.ty = t.clone();
                            symbol.ty.clone()
                        }
                        None => return Err(span.error(ErrorKind::Type, format!("Can't infer type for {}", symbol.name))),
                    }
                    ty => match var_type {
                        Some(t) => if ty != t {
                            return Err(span.error(ErrorKind::Type, format!("Type mismatch for {}", symbol.name)))
                        } else {
                            ty.clone()
                        },
                        None => { 
                            self.variables.insert(symbol.name.clone(), symbol.ty.clone());
                            ty.clone()
                        },
                    }
                }
            },
            ExpressionKind::Array(list) => {
                let mut ty = Type::Inferred;
                for expr in list.iter_mut() {
                    let el_ty = self.check_expression(expr)?;
                    if ty == Type::Inferred {
                        ty = el_ty.clone();
                    }
                    if ty != el_ty {
                        return Err(expr.span.error(ErrorKind::Type, format!("Mismatch types for array elements, expected: {:?}", el_ty)));
                    }
                }
                Type::Array(Box::new(ty), list.len())
            },
            ExpressionKind::Index(collection, index, var_ty, coll_length) => {
                if self.check_expression(index)? != Type::Int {
                    return Err(expr.span.error(ErrorKind::Type, "Expected int as index".into()));
                }
                match self.check_expression(collection)? {
                    Type::Array(ty, length) => {
                        *var_ty = (*ty).clone();
                        *coll_length = length as u32;
                        *ty
                    },
                    _ => return Err(expr.span.error(ErrorKind::Type, "Expected array to index into".into())),
                }
            },
            ExpressionKind::Match(expr, ty, cases) => {
                *ty = self.check_expression(expr)?;
                let ty = cases.iter_mut()
                    .map(|(pattern, exprs)|{
                        if !pattern.matches_on(&ty) {
                            return Err(expr.span.error(ErrorKind::Type, "Unexpected pattern".into()));
                        }
                        let ty = self.check_group(exprs)?;
                        Ok(ty)
                    })
                    .try_fold(None, |acc, ty| {
                        match acc {
                            None => Ok(Some(ty?)),
                            Some(acc) => {
                                if acc != ty? {
                                    return Err(expr.span.error(ErrorKind::Type, "Type of case blocks do not match".into()));
                                }
                                Ok(Some(acc))
                            }
                        }
                    })?.unwrap();
                ty
            },
            ExpressionKind::Case(..) => unreachable!(),
        };
        self.data_types.check_named_type(&mut ty)?;
        Ok(ty)
    }
}