
use std::{collections::{HashMap, hash_map::Entry}};

use itertools::Itertools;

use crate::{parser::{SyntaxTree, Type, Fn, Expression, ExpressionKind, Signature, BinOpKind}, error::{Result, Error, ThrowablePosition, ErrorKind}, lexer::{Literal, Span, Position}};


pub fn type_check(syntax_tree: &mut SyntaxTree) -> Result<()> {
    let mut data_map = HashMap::new();
    syntax_tree.data_types()
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

    for fun in syntax_tree.fns_impls_mut() {
        TypeCheck::new(&functions, &data_map).check_fun(fun)?
    }
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
}

impl<'f> TypeCheck<'f> {
    fn new(functions: &'f HashMap<String, Signature>, data_types: &'f HashMap<String, (Type, Span)>) -> TypeCheck<'f> {
        TypeCheck {
            variables: HashMap::new(),
            functions,
            data_types,
        }
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
                self.check_expression(expr)?;
                Type::Void
            },
            ExpressionKind::Signature(_) => {
                todo!()
            }
            ExpressionKind::Fn(fun) => {
                self.check_fun(fun)?;
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
            ExpressionKind::Field(expr, field_name, field_type, field_offset) => {
                let ty = self.check_expression(expr)?;

                match ty {
                    Type::Struct(_, map) => {
                        let Some((ty, offset)) = map.get(field_name) else { 
                            return Err(expr.span.error(ErrorKind::Type, format!("No field found with this name: {}", field_name))); 
                        };

                        *field_type = ty.to_owned();
                        *field_offset = *offset;
                        ty.to_owned()
                    },
                    _ => return Err(expr.span.error(ErrorKind::Type, format!("No field for type: {:?}", ty)))
                }
            }
            ExpressionKind::Let(name, expr, let_ty) => {
                let ty = self.check_expression(expr)?;
                if *let_ty != Type::Inferred && *let_ty != ty {
                    return Err(expr.span.error(ErrorKind::Type, format!("Mismatch types for assignment, expected: {:?}", let_ty)));
                }
                self.variables.insert(name.clone(), ty.clone());
                *let_ty = ty.clone();
                ty
            },
            ExpressionKind::Mut(name, field, expr) => {
                let var_type = self.variables.get(name);
                if let Some(var_ty) = var_type.cloned() {
                    let ty = if let Some(field) = field {
                        if let Type::Struct(_, fields) = var_ty.clone() {
                            if let Some((ty, offset)) = fields.get(&field.0) {
                                field.1 = *offset;
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
                    return Err(expr.span.error(ErrorKind::Type, format!("No variable found with this name: {:?}", name)));
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
            ExpressionKind::For(iter, var_name, var_type, for_body) => {
                let ty = self.check_expression(iter)?;
                if let Type::Iter(arr) = ty {
                    if let Type::Array(arr_type, _) = *arr {
                        if *var_type == Type::Inferred {
                            *var_type = *arr_type.clone();
                        }
                        self.variables.insert(var_name.clone(), var_type.clone());
                        if *arr_type != *var_type {
                            return Err(expr.span.error(ErrorKind::Type, "Array type and for type do not match".to_string()));
                        }
                        for expr in for_body {
                            self.check_expression(expr)?;
                        }
                        Type::Void
                    } else {
                        return Err(expr.span.error(ErrorKind::Type, "Expected array as iterator".to_string()));
                    }
                } else {
                    return Err(expr.span.error(ErrorKind::Type, "Expected iter as input for for block".to_string()));
                }
            },
            ExpressionKind::Iter(iter, len) => {
                let ty = self.check_expression(iter)?;
                match ty {
                    Type::Array(_, l) => {
                        *len = l as u32;
                        Type::Iter(Box::new(ty))
                    },
                    _ => return Err(expr.span.error(ErrorKind::Type, "Expected array as iterable".to_string())),
                }
            },
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
            ExpressionKind::Symbol(name, ty) => {
                let var_type = self.variables.get(name);
                match ty {
                    Type::Inferred => match var_type {
                        Some(t) => {
                            *ty = t.clone();
                            ty.clone()
                        }
                        None => return Err(span.error(ErrorKind::Type, format!("Can't infer type for {}", name))),
                    }
                    ty => match var_type {
                        Some(t) => if ty != t {
                            return Err(span.error(ErrorKind::Type, format!("Type mismatch for {}", name)))
                        } else {
                            ty.clone()
                        },
                        None => { 
                            self.variables.insert(name.clone(), ty.clone());
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