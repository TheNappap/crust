use std::collections::HashMap;

use crate::{parser::{SyntaxTree, Type, Fn, Expression}, error::{Result, Error}};

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
            Expression::Let(name, value) => {
                let ty = match value {
                    crate::lexer::Value::Int(_) => Type::Int,
                    crate::lexer::Value::Float(_) => Type::Float,
                    crate::lexer::Value::String(_) => Type::String,
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