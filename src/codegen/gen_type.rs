
use cranelift_codegen::ir::{self, types::I8, AbiParam};
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use cranelift_codegen::ir::types::{I64, F64};
use itertools::Itertools;

use crate::{parser::Type, error::{Result}};

pub enum GenTypeKind {
    Type(ir::Type),
    Vec(Vec<GenType>),
}

pub struct GenType {
    kind: GenTypeKind,
    size: u32,
}

impl GenType {
    pub fn from_type(ty: &Type, module: &ObjectModule) -> Result<GenType> {
        let kind = match ty {
            Type::Int => GenTypeKind::Type(I64),
            Type::Float => GenTypeKind::Type(F64),
            Type::Bool => GenTypeKind::Type(I8),
            Type::String => GenTypeKind::Type(module.target_config().pointer_type()),
            Type::Array(ty, len) => GenType::kind_from_types((0..*len).map(|_| *ty.clone()).collect(), module)?,
            Type::Range(_) => GenType::kind_from_types((0..2).map(|_| Type::Int).collect(), module)?,
            Type::Struct(_, types) => GenType::kind_from_types(types.values().map(|t| t.0.clone()).collect(), module)?,
            Type::Enum(_, _) => GenTypeKind::Type(I64),
            Type::Iter(_) => GenTypeKind::Type(module.target_config().pointer_type()),
            Type::Void => GenTypeKind::Vec(vec![]),
            Type::Inferred | Type::Named(_) => unreachable!(),
        };
        Ok(GenType{ kind, size: ty.size() })
    }

    fn kind_from_types(types: Vec<Type>, module: &ObjectModule) -> Result<GenTypeKind> {
        let types: Vec<_> = types.into_iter()
            .map(|ty| GenType::from_type(&ty, module))
            .try_collect()?;
        Ok(GenTypeKind::Vec(types))
    }

    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn types(&self) -> Vec<&ir::Type> {
        match &self.kind {
            GenTypeKind::Type(ty) => vec![ty],
            GenTypeKind::Vec(tys) => tys.iter()
                .map(|ty| ty.types())
                .flatten()
                .collect()
        }
    }

    pub fn offsets(&self) -> Vec<i32> {
        match &self.kind {
            GenTypeKind::Type(_) => vec![0],
            GenTypeKind::Vec(tys) => tys.iter()
                .scan(0, |s, ty| {
                    let cur_offset = *s;
                    *s += ty.size() as i32;
                    Some(ty.offsets().iter()
                        .map(|offset| cur_offset+*offset)
                        .collect_vec())
                })
                .flatten()
                .collect(),
        }
    }

    pub fn add_to_params(self, params: &mut Vec<AbiParam>) {
        match self.kind {
            GenTypeKind::Type(ty) => params.push(AbiParam::new(ty)),
            GenTypeKind::Vec(tys) => tys.into_iter().for_each(|ty |ty.add_to_params(params)),
        }
    }
}