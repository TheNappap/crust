
use crate::{block_definitions::OperatorBlockDefintion, parser::{BinOpKind, BlockTag, OperatorKind, UnOpKind}};

#[derive(Default)]
pub struct Not;

impl OperatorBlockDefintion for Not {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Not)
    }

    fn unary_operator(&self) -> Option<UnOpKind> {
        Some(UnOpKind::Neg)
    }
}

#[derive(Default)]
pub struct Add;

impl OperatorBlockDefintion for Add {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Plus)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Add)
    }
}

#[derive(Default)]
pub struct Dash;

impl OperatorBlockDefintion for Dash {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Dash)
    }

    fn unary_operator(&self) -> Option<UnOpKind> {
        Some(UnOpKind::Neg)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Sub)
    }
}

#[derive(Default)]
pub struct Multiply;

impl OperatorBlockDefintion for Multiply {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Star)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Mul)
    }
}

#[derive(Default)]
pub struct Divide;

impl OperatorBlockDefintion for Divide {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Slash)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Div)
    }
}

#[derive(Default)]
pub struct Equals;

impl OperatorBlockDefintion for Equals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::EqEq)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Eq)
    }
}

#[derive(Default)]
pub struct NotEquals;

impl OperatorBlockDefintion for NotEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Neq)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Neq)
    }
}

#[derive(Default)]
pub struct LessThan;

impl OperatorBlockDefintion for LessThan {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Less)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Less)
    }
}

#[derive(Default)]
pub struct LessEquals;

impl OperatorBlockDefintion for LessEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::LessEq)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::LessEq)
    }
}

#[derive(Default)]
pub struct GreatThan;

impl OperatorBlockDefintion for GreatThan {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::Great)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::Great)
    }
}

#[derive(Default)]
pub struct GreatEquals;

impl OperatorBlockDefintion for GreatEquals {
    fn id(&self) -> BlockTag {
        BlockTag::Operator(OperatorKind::GreatEq)
    }

    fn binary_operator(&self) -> Option<BinOpKind> {
        Some(BinOpKind::GreatEq)
    }
}