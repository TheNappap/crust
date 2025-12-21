use std::{collections::HashMap, rc::Rc};

use itertools::Itertools;

use crate::{
    lexer::{Delimeter, Span, Token, TokenKind}, parser::{BinOpKind, BlockTag, Expression, ExpressionKind, Parser, Type, UnOpKind}, utils::{ErrorKind, Result, ThrowablePosition}
};

pub mod dot;
pub mod call;
pub mod path;
pub mod fn_def;
pub mod print;
pub mod assign;
pub mod operators;
pub mod returns;
pub mod bools;
pub mod conditional;
pub mod loops;
pub mod group;
pub mod array;
pub mod iter;
pub mod iter_transforms;
pub mod range;
pub mod data;
pub mod traits;
pub mod pattern_match;

pub trait BlockDefinition {
    fn id(&self) -> BlockTag;
    fn parse(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind>;
    fn parse_chained(&self, span: &Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind>;

    fn parse_expression(&self, span: Span, header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<Expression> {
        self.parse(&span, header, body, parser)
            .map(|kind| Expression::new(kind, span))
    }
    fn parse_chained_expression(&self, span: Span, header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<Expression> {
        self.parse_chained(&span, header, body, input, parser)
            .map(|kind| Expression::new(kind, span))
    }
}

pub trait OperatorBlockDefintion : BlockDefinition {
    fn id(&self) -> BlockTag;
    fn unary_operator(&self) -> Option<UnOpKind> { None }
    fn binary_operator(&self) -> Option<BinOpKind> { None }

    fn parse_unary_operator(&self, span: &Span, operand: Expression, trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        assert!(trailing_groups.is_empty() || self.allow_trailing_groups());
        let Some(op_kind) = self.unary_operator() else {
            return span.syntax("Operator cannot be used as unary operator".into());
        };
        Ok( ExpressionKind::UnOp(op_kind, Box::new(operand), Type::Inferred) )
    }
    fn parse_binary_operator(&self, span: &Span, operand1: Expression, operand2: Expression, trailing_groups: Vec<TrailingGroup>) -> Result<ExpressionKind> {
        assert!(trailing_groups.is_empty() || self.allow_trailing_groups());
        let Some(op_kind) = self.binary_operator() else {
            return span.syntax("Operator cannot be used as binary operator".into());
        };
        Ok( ExpressionKind::BinOp(op_kind, Box::new(operand1), Box::new(operand2), Type::Inferred) )
    }

    fn allow_trailing_groups(&self) -> bool {
        false
    }
}

impl<T: OperatorBlockDefintion> BlockDefinition for T {
    // TODO force Operator as BlockTag
    fn id(&self) -> BlockTag {
        OperatorBlockDefintion::id(self)
    }

    fn parse(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let trailing_groups = TrailingGroup::collect_trailing_groups(&mut header, parser, self.allow_trailing_groups())?;
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        match operands.len() {
            1 => self.parse_unary_operator(span, operands.remove(0), trailing_groups),
            2 => self.parse_binary_operator(span, operands.remove(0), operands.remove(0), trailing_groups),
            _ => span.syntax("Operator expects 1 or 2 operands".into())
        }
    }

    fn parse_chained(&self, span: &Span, mut header: Vec<Token>, body: Vec<Token>, input: Expression, parser: &Parser) -> Result<ExpressionKind> {
        assert!(body.is_empty());
        let trailing_groups = TrailingGroup::collect_trailing_groups(&mut header, parser, self.allow_trailing_groups())?;
        let mut operands: Vec<_> = parser.iter_expression(header).try_collect()?;
        match operands.len() {
            1 => self.parse_binary_operator(span, input, operands.remove(0), trailing_groups),
            _ => span.syntax("Operator expects 1 or 2 operands".into())
        }
    }
}

pub struct TrailingGroup {
    delimeter: Delimeter,
    expressions: Vec<Expression>,
}

impl TrailingGroup {
    fn expressions(self) -> Vec<Expression> {
        self.expressions
    }

    fn collect_trailing_groups(tokens: &mut Vec<Token>, parser: &Parser, do_collect: bool) -> Result<Vec<Self>> {
        if !do_collect {
            return Ok(vec![]);
        }

        let mut trailing_groups = vec![];
        while let Some(Token { kind: TokenKind::Group(Delimeter::Brackets | Delimeter::Parens, _), .. }) = tokens.last() {
            let group = tokens.pop().unwrap();
            let group = match group.kind {
                TokenKind::Group(delimeter, tokens) => {
                    Self { delimeter, expressions: parser.iter_expression(tokens).try_collect()? }
                },
                _ => unreachable!()
            };
            trailing_groups.push(group);
        }
        Ok(trailing_groups)
    }
}

pub struct BlockDefinitions {
    definitions: HashMap<BlockTag, Rc<dyn BlockDefinition>>,
}

impl BlockDefinitions {
    pub fn new() -> BlockDefinitions {
        BlockDefinitions {
            definitions: HashMap::new(),
        }
    }

    pub fn get(&self, tag: &BlockTag, span: &Span) -> Result<Rc<dyn BlockDefinition>> {
        self.definitions
            .get(tag)
            .cloned()
            .ok_or(span.error(ErrorKind::Syntax, format!("Definition for block not found: {}", tag)))
    }

    pub fn add<T: BlockDefinition + Default + 'static>(&mut self) {
        let definition: Rc<dyn BlockDefinition> = Rc::new(T::default());
        let key = definition.id();
        if self.definitions.contains_key(&key) {
            println!("Definition of block \"{}\" was overidden.", key);
        }
        self.definitions
            .insert(definition.id(), definition);
    }
}

impl Default for BlockDefinitions {
    fn default() -> Self {
        let mut blockdefs = BlockDefinitions::new();
        blockdefs.add::<dot::Dot>();
        blockdefs.add::<call::Call>();
        blockdefs.add::<path::PathBlock>();
        blockdefs.add::<returns::Return>();
        blockdefs.add::<returns::Forward>();
        blockdefs.add::<fn_def::FnDef>();
        blockdefs.add::<fn_def::Impl>();
        blockdefs.add::<traits::TraitBlock>();
        blockdefs.add::<data::Struct>();
        blockdefs.add::<data::Enum>();
        blockdefs.add::<data::New>();
        blockdefs.add::<data::Field>();
        blockdefs.add::<print::Print>();
        blockdefs.add::<print::PrintLn>();
        blockdefs.add::<assign::Let>();
        blockdefs.add::<assign::Mut>();
        blockdefs.add::<operators::Not>();
        blockdefs.add::<operators::Add>();
        blockdefs.add::<operators::Dash>();
        blockdefs.add::<operators::Multiply>();
        blockdefs.add::<operators::Divide>();
        blockdefs.add::<operators::Equals>();
        blockdefs.add::<operators::NotEquals>();
        blockdefs.add::<operators::LessThan>();
        blockdefs.add::<operators::LessEquals>();
        blockdefs.add::<operators::GreatThan>();
        blockdefs.add::<operators::GreatEquals>();
        blockdefs.add::<bools::True>();
        blockdefs.add::<bools::False>();
        blockdefs.add::<conditional::If>();
        blockdefs.add::<conditional::Else>();
        blockdefs.add::<loops::While>();
        blockdefs.add::<loops::For>();
        blockdefs.add::<loops::Fold>();
        blockdefs.add::<group::Group>();
        blockdefs.add::<array::Array>();
        blockdefs.add::<array::Index>();
        blockdefs.add::<iter::Iter>();
        blockdefs.add::<iter_transforms::Map>();
        blockdefs.add::<iter_transforms::Filter>();
        blockdefs.add::<range::Range>();
        blockdefs.add::<pattern_match::Match>();
        blockdefs.add::<pattern_match::Case>();
        blockdefs
    }
}
