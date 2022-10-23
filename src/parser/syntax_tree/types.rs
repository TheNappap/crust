use crate::lexer::Token;


#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Inferred,
    Array(Box<Type>, usize),
    //Struct(Vec<Type>, usize),
}

impl From<Token> for Type {
    fn from(token: Token) -> Self {
        match token {
            Token::Ident(ty) => match ty.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                _ty => todo!()
            }
            Token::Literal(_) => todo!(),
            Token::Symbol(_) => todo!(),
            Token::Operator(_) => todo!(),
            Token::Group(_, _) => todo!(),
            Token::NewLine => todo!(),
        }
    }
}