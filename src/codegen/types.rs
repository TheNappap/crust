
use cranelift_codegen::ir::{self, AbiParam};
use cranelift_module::Module;
use cranelift_object::ObjectModule;
use cranelift_codegen::ir::types::{I64, F64, B64};
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
        let (kind, size) = match ty {
            Type::Int => (GenTypeKind::Type(I64), 8),
            Type::Float => (GenTypeKind::Type(F64), 8),
            Type::Bool => (GenTypeKind::Type(B64), 8),
            Type::String => (GenTypeKind::Type(module.target_config().pointer_type()), 8),
            Type::Array(ty, len) => GenType::from_types((0..*len).map(|_| *ty.clone()).collect(), module)?,
            Type::Struct(types) => GenType::from_types(types.values().cloned().collect(), module)?,
            Type::Iter(_) => (GenTypeKind::Type(module.target_config().pointer_type()), 8),
            Type::Void => (GenTypeKind::Vec(vec![]), 0),
            Type::Inferred | Type::Named(_) => unreachable!(),
        };
        Ok(GenType{ kind, size })
    }

    fn from_types(types: Vec<Type>, module: &ObjectModule) -> Result<(GenTypeKind, u32)> {
        let types: Vec<_> = types.into_iter()
            .map(|ty| GenType::from_type(&ty, module))
            .try_collect()?;
        let size = types.iter().fold(0,|acc, ty| acc+ty.size);
        Ok((GenTypeKind::Vec(types), size))
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