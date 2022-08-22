use std::collections::HashMap;

use crate::{parser::{SyntaxTree, Type, Fn, Expression}, error::{Result, Error}, lexer::Literal};

pub fn type_check(syntax_tree: &mut SyntaxTree) -> Result<()> {
    for fun in syntax_tree.fns_mut() {
        TypeCheck::new().check_fun(fun)?
    }
    Ok(())
}

struct TypeCheck {
    variables: HashMap<String, Type>
}

impl TypeCheck {
    fn new() -> TypeCheck {
        TypeCheck {
            variables: HashMap::new()
        }
    }

    fn check_fun(&mut self, fun: &mut Fn) -> Result<()> {
        for expr in fun.expressions_mut() {
            self.check_expression(expr)?;
        }
        Ok(())
    }

    fn check_expression(&mut self, expr: &mut Expression) -> Result<Type> {
        let ty = match expr {
            Expression::Call(signature, params) => {
                for expr in params {
                    self.check_expression(expr)?;
                }
                signature.returns().clone()
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
            Expression::Literal(Literal::Int(_)) => Type::Int,
            Expression::Literal(Literal::Float(_)) => Type::Float,
            Expression::Literal(Literal::String(_)) => Type::String,
            Expression::AddrOf(expr) => self.check_expression(expr)?,
            Expression::Add(param1, param2, add_ty) => {
                let ty = self.check_expression(param1)?;
                if *add_ty != Type::Inferred && *add_ty != ty {
                    return Err(Error::type_(format!("Mismatch types for binary operation, expected: {:?}", add_ty), 0));
                }
                if ty != self.check_expression(param2)? {
                    return Err(Error::type_(format!("Mismatch types for binary operation, expected: {:?}", ty), 0));
                }
                *add_ty = ty.clone();
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