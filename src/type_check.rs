use std::collections::HashMap;

use crate::{parser::{SyntaxTree, Type, Fn, Expression, Signature}, error::{Result, Error}, lexer::Literal};

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
                      .collect::<Result<Vec<_>>>()?;

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
            Expression::BinOp(_, param1, param2, op_ty) => {
                let ty = self.check_expression(param1)?;
                if *op_ty != Type::Inferred && *op_ty != ty {
                    return Err(Error::type_(format!("Mismatch types for binary operation, expected: {:?}", op_ty), 0));
                }
                if ty != self.check_expression(param2)? {
                    return Err(Error::type_(format!("Mismatch types for binary operation, expected: {:?}", ty), 0));
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
        };
        Ok(ty)
    }
}