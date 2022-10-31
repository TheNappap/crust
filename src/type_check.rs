use std::collections::HashMap;

use crate::{parser::{SyntaxTree, Type, Fn, Expression, Signature, BinOpKind}, error::{Result, Error}, lexer::Literal};

pub fn type_check(syntax_tree: &mut SyntaxTree) -> Result<()> {
    let mut functions = HashMap::new();
    for fun in syntax_tree.fns() {
        for expr in fun.expressions() {
            if let Expression::Fn(f) = expr {
                functions.insert(f.signature().name().to_string(), f.signature().clone());
            }
        }
        functions.insert(fun.signature().name().to_string().clone(), fun.signature().clone()); 
    }

    for fun in syntax_tree.fns_mut() {
        TypeCheck::new(&functions).check_fun(fun)?
    }
    Ok(())
}

struct TypeCheck<'f> {
    variables: HashMap<String, Type>,
    functions: &'f HashMap<String, Signature>
}

impl<'f> TypeCheck<'f> {
    fn new(functions: &HashMap<String, Signature>) -> TypeCheck {
        TypeCheck {
            variables: HashMap::new(),
            functions
        }
    }

    fn check_fun(&mut self, fun: &mut Fn) -> Result<()> {
        for (name, ty) in fun.params().zip(fun.signature().params()) {
            self.variables.insert(name.clone(), ty.clone());
        }
        for expr in fun.expressions_mut() {
            self.check_expression(expr)?;
        }
        Ok(())
    }

    fn check_expression(&mut self, expr: &mut Expression) -> Result<Type> {
        let ty = match expr {
            Expression::Call(signature, params) => {
                params.iter_mut()
                      .map(|expr| self.check_expression(expr))
                      .zip(signature.params())
                      .map(|(r,t)| {
                            r.and_then(|ty| {
                                if *t != ty {
                                    return Err(Error::type_("Mismatched types for parameter".to_string(), 0));
                                }
                                Ok(ty)
                            })
                      })
                      .try_for_each(|x| x.and_then(|_| Ok(())))?;

                if *signature.returns() == Type::Inferred {
                    let fun = self.functions.get(signature.name()).expect("Function not found");
                    *signature = fun.clone();
                }
                signature.returns().clone()
            },
            Expression::Return(expr) => {
                self.check_expression(expr)?;
                Type::Void
            },
            Expression::Fn(fun) => {
                self.check_fun(fun)?;
                Type::Inferred
            }
            Expression::Let(name, expr, let_ty) => {
                let ty = self.check_expression(expr)?;
                if *let_ty != Type::Inferred && *let_ty != ty {
                    return Err(Error::type_(format!("Mismatch types for assignment, expected: {:?}", let_ty), 0));
                }
                self.variables.insert(name.clone(), ty.clone());
                *let_ty = ty.clone();
                ty
            },
            Expression::Mut(name, expr) => {
                let ty = self.check_expression(expr)?;
                let var_type = self.variables.get(name);
                if let Some(var_ty) = var_type {
                    if *var_ty != Type::Inferred && *var_ty != ty {
                        return Err(Error::type_(format!("Mismatch types for assignment, expected: {:?}", var_ty), 0));
                    }
                    ty
                } else {
                    return Err(Error::type_(format!("No variable found with this name: {:?}", name), 0));
                }
            },
            Expression::If(condition, if_body, else_body) => {
                let ty = self.check_expression(condition)?;
                if ty != Type::Bool {
                    return Err(Error::type_("Expected a boolean type as condition".to_string(), 0));
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
            Expression::While(condition, while_body) => {
                let ty = self.check_expression(condition)?;
                if ty != Type::Bool {
                    return Err(Error::type_("Expected a boolean type as condition".to_string(), 0));
                }
                for expr in while_body {
                    self.check_expression(expr)?;
                }
                Type::Void
            },
            Expression::For(iter, var_name, var_type, for_body) => {
                let ty = self.check_expression(iter)?;
                if let Type::Iter(arr) = ty {
                    if let Type::Array(arr_type, _) = *arr {
                        if *var_type == Type::Inferred {
                            *var_type = *arr_type.clone();
                        }
                        self.variables.insert(var_name.clone(), var_type.clone());
                        if *arr_type != *var_type {
                            return Err(Error::type_("Array type and for type do not match".to_string(), 0));
                        }
                        for expr in for_body {
                            self.check_expression(expr)?;
                        }
                        Type::Void
                    } else {
                        return Err(Error::type_("Expected array as iterator".to_string(), 0));
                    }
                } else {
                    return Err(Error::type_("Expected iter as input for for block".to_string(), 0));
                }
            },
            Expression::Iter(iter, len) => {
                let ty = self.check_expression(iter)?;
                match ty {
                    Type::Array(_, l) => {
                        *len = l as u32;
                        Type::Iter(Box::new(ty))
                    },
                    _ => return Err(Error::type_("Expected array as iterable".to_string(), 0)),
                }
            },
            Expression::Literal(Literal::Int(_)) => Type::Int,
            Expression::Literal(Literal::Float(_)) => Type::Float,
            Expression::Literal(Literal::Bool(_)) => Type::Bool,
            Expression::Literal(Literal::String(_)) => Type::String,
            Expression::AddrOf(exprs) => {
                for expr in exprs {
                    self.check_expression(expr)?;
                }
                Type::Int
            },
            Expression::BinOp(kind, param1, param2, op_ty) => {
                let ty = self.check_expression(param1)?;
                if ty != self.check_expression(param2)? {
                    return Err(Error::type_(format!("Mismatch types for operands, expected: {:?}", ty), 0));
                }

                let out_ty = match kind {
                    BinOpKind::Eq | BinOpKind::Neq => Type::Bool,
                    _ => ty.clone()
                };

                if *op_ty != Type::Inferred && *op_ty != out_ty {
                    return Err(Error::type_(format!("Mismatch types for binary operation, expected: {:?}", op_ty), 0));
                }
                *op_ty = ty;
                out_ty
            },
            Expression::UnOp(_, param, op_ty) => {
                let ty = self.check_expression(param)?;
                if *op_ty != Type::Inferred && *op_ty != ty {
                    return Err(Error::type_(format!("Mismatch types for unary operation, expected: {:?}", op_ty), 0));
                }
                *op_ty = ty.clone();
                ty
            },
            Expression::Symbol(name, ty) => {
                let var_type = self.variables.get(name);
                match ty {
                    Type::Inferred => match var_type {
                        Some(t) => {
                            *ty = t.clone();
                            ty.clone()
                        }
                        None => return Err(Error::type_(format!("Can't infer type for {}", name), 0)),
                    }
                    ty => match var_type {
                        Some(t) => if ty != t {
                            return Err(Error::type_(format!("Type mismatch for {}", name), 0))
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
            Expression::Array(list) => {
                let mut ty = Type::Inferred;
                for expr in list.iter_mut() {
                    let el_ty = self.check_expression(expr)?;
                    if ty == Type::Inferred {
                        ty = el_ty.clone();
                    }
                    if ty != el_ty {
                        return Err(Error::type_(format!("Mismatch types for array elements, expected: {:?}", el_ty), 0));
                    }
                }
                Type::Array(Box::new(ty), list.len())
            },
            Expression::Index(collection, index, var_ty, coll_length) => {
                if self.check_expression(index)? != Type::Int {
                    return Err(Error::type_("Expected int as index".into(), 0));
                }
                match self.check_expression(collection)? {
                    Type::Array(ty, length) => {
                        *var_ty = (*ty).clone();
                        *coll_length = length as u32;
                        *ty
                    },
                    _ => return Err(Error::type_("Expected array to index into".into(), 0)),
                }
            },
        };
        Ok(ty)
    }
}