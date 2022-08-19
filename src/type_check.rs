use std::collections::HashMap;

use crate::{parser::{SyntaxTree, Type, Fn, Expression, Literal}, error::{Result, Error}};

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
            self.check_expression(expr)?
        }
        Ok(())
    }

    fn check_expression(&mut self, expr: &mut Expression) -> Result<()> {
        match expr {
            Expression::Call(_, params, _) => {
                for expr in params {
                    self.check_expression(expr)?
                }
            },
            Expression::Fn(fun) => self.check_fun(fun)?,
            Expression::Let(name, expr) => {
                let ty = match **expr {
                    Expression::Literal(Literal::Int(_)) => Type::Int,
                    Expression::Literal(Literal::Float(_)) => Type::Float,
                    Expression::Literal(Literal::String(_)) => Type::String,
                    Expression::Call(_, _, _) => Type::Inferred,
                    Expression::Fn(_) => Type::Inferred,
                    Expression::Let(_, _) => Type::Inferred,
                    Expression::Pointer(_) => Type::Inferred,
                    Expression::Symbol(_, _) => Type::Inferred,
                };
                self.variables.insert(name.clone(), ty);
            },
            Expression::Literal(_) => (),
            Expression::Pointer(_) => (),
            Expression::Symbol(name, ty) => {
                let var_type = self.variables.get(name);
                match ty {
                    Type::Inferred => match var_type {
                        Some(t) => *ty = t.clone(),
                        None => return Err(Error::type_(format!("Can't infer type for {}", name), 0)),
                    }
                    ty => match var_type {
                        Some(t) => if ty != t {
                            return Err(Error::type_(format!("Type mismatch for {}", name), 0))
                        },
                        None => { self.variables.insert(name.clone(), ty.clone()); },
                    }
                }
            },
        };
        Ok(())
    }
}