use crate::_type::{TypeBuilder, TypeRegistry};
use crate::string_pool::StringPool;
use std::rc::Rc;
use crate::instance::{Function, Instance};
use crate::vm::{NewVM};
use crate::instance::Instance::{Byte, UByte, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float32, Float64};

pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
    let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Number"))
        .supertype(type_registry.get(Rc::new("silicon.core.Object".to_string())))
        .build();
    type_registry.register(_type)
}

pub(crate) mod byte {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Byte"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod ubyte {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.UByte"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod int16 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Int16"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod uint16 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.UInt16"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod int32 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Int32"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod uint32 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.UInt32"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod int64 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Int64"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod uint64 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.UInt64"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod int128 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Int128"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod uint128 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.UInt128"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod float32 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Float32"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}

pub(crate) mod float64 {
    use crate::_type::{TypeBuilder, TypeRegistry};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;

    pub(crate) fn create(string_pool: &mut StringPool, type_registry: &mut TypeRegistry) {
        let _type = TypeBuilder::new(string_pool.pool_str("silicon.core.Float64"))
            .supertype(type_registry.get(Rc::new("silicon.core.Number".to_string())))
            .build();
        type_registry.register(_type)
    }
}