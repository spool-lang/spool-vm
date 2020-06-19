use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
use crate::string_pool::StringPool;
use std::rc::Rc;
use crate::instance::{Function, Instance};
use crate::vm::{VM};
use crate::instance::Instance::{Byte, UByte, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, Float32, Float64};
use crate::instance::Function::NativeInstance;
use std::cell::RefCell;

pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
    let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Number"))
        .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.Object")))
        .instance_function(string_pool.borrow_mut().pool_str("toByte"), 0, to_byte)
        .instance_function(string_pool.borrow_mut().pool_str("toUByte"), 0, to_ubyte)
        .instance_function(string_pool.borrow_mut().pool_str("toInt16"), 0, to_int16)
        .instance_function(string_pool.borrow_mut().pool_str("toUInt16"), 0, to_uint16)
        .instance_function(string_pool.borrow_mut().pool_str("toInt32"), 0, to_int32)
        .instance_function(string_pool.borrow_mut().pool_str("toUInt32"), 0, to_uint32)
        .instance_function(string_pool.borrow_mut().pool_str("toInt64"), 0, to_int64)
        .instance_function(string_pool.borrow_mut().pool_str("toUInt64"), 0, to_uint64)
        .instance_function(string_pool.borrow_mut().pool_str("toInt128"), 0, to_int128)
        .instance_function(string_pool.borrow_mut().pool_str("toUInt128"), 0, to_uint128)
        .instance_function(string_pool.borrow_mut().pool_str("toFloat32"), 0, to_float32)
        .instance_function(string_pool.borrow_mut().pool_str("toFloat64"), 0, to_float64)
        .build();
    type_registry.borrow_mut().register(_type)
}

fn to_byte(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num,
        Instance::Int16(num) => num as i8,
        Instance::Int32(num) => num as i8,
        Instance::Int64(num) => num as i8,
        Instance::Int128(num) => num as i8,
        Instance::Float32(num) => num as i8,
        Instance::Float64(num) => num as i8,
        _ => panic!()
    };
    Byte(value)
}

fn to_ubyte(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::UByte(num) => num,
        Instance::UInt16(num) => num as u8,
        Instance::UInt32(num) => num as u8,
        Instance::UInt64(num) => num as u8,
        Instance::UInt128(num) => num as u8,
        _ => panic!()
    };
    UByte(value)
}

fn to_int16(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as i16,
        Instance::Int16(num) => num,
        Instance::Int32(num) => num as i16,
        Instance::Int64(num) => num as i16,
        Instance::Int128(num) => num as i16,
        Instance::Float32(num) => num as i16,
        Instance::Float64(num) => num as i16,
        _ => panic!()
    };
    Int16(value)
}

fn to_uint16(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::UByte(num) => num as u16,
        Instance::UInt16(num) => num,
        Instance::UInt32(num) => num as u16,
        Instance::UInt64(num) => num as u16,
        Instance::UInt128(num) => num as u16,
        _ => panic!()
    };
    UInt16(value)
}

fn to_int32(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as i32,
        Instance::Int16(num) => num as i32,
        Instance::Int32(num) => num,
        Instance::Int64(num) => num as i32,
        Instance::Int128(num) => num as i32,
        Instance::Float32(num) => num as i32,
        Instance::Float64(num) => num as i32,
        _ => panic!()
    };
    Int32(value)
}

fn to_uint32(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::UByte(num) => num as u32,
        Instance::UInt16(num) => num as u32,
        Instance::UInt32(num) => num,
        Instance::UInt64(num) => num as u32,
        Instance::UInt128(num) => num as u32,
        _ => panic!()
    };
    UInt32(value)
}

fn to_int64(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as i64,
        Instance::Int16(num) => num as i64,
        Instance::Int32(num) => num as i64,
        Instance::Int64(num) => num,
        Instance::Int128(num) => num as i64,
        Instance::Float32(num) => num as i64,
        Instance::Float64(num) => num as i64,
        _ => panic!()
    };
    Int64(value)
}

fn to_uint64(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::UByte(num) => num as u64,
        Instance::UInt16(num) => num as u64,
        Instance::UInt32(num) => num as u64,
        Instance::UInt64(num) => num,
        Instance::UInt128(num) => num as u64,
        _ => panic!()
    };
    UInt64(value)
}

fn to_int128(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as i128,
        Instance::Int16(num) => num as i128,
        Instance::Int32(num) => num as i128,
        Instance::Int64(num) => num as i128,
        Instance::Int128(num) => num,
        Instance::Float32(num) => num as i128,
        Instance::Float64(num) => num as i128,
        _ => panic!()
    };
    Int128(value)
}

fn to_uint128(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::UByte(num) => num as u128,
        Instance::UInt16(num) => num as u128,
        Instance::UInt32(num) => num as u128,
        Instance::UInt64(num) => num as u128,
        Instance::UInt128(num) => num,
        _ => panic!()
    };
    UInt128(value)
}

fn to_float32(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as f32,
        Instance::Int16(num) => num as f32,
        Instance::Int32(num) => num as f32,
        Instance::Int64(num) => num as f32,
        Instance::Int128(num) => num as f32,
        Instance::UByte(num) => num as f32,
        Instance::UInt16(num) => num as f32,
        Instance::UInt32(num) => num as f32,
        Instance::UInt64(num) => num as f32,
        Instance::UInt128(num) => num as f32,
        Instance::Float32(num) => num,
        Instance::Float64(num) => num as f32,
        _ => panic!()
    };
    Float32(value)
}

fn to_float64(vm: &mut VM, instance: Instance, args: Vec<Instance>) -> Instance {
    let value = match instance {
        Instance::Byte(num) => num as f64,
        Instance::Int16(num) => num as f64,
        Instance::Int32(num) => num as f64,
        Instance::Int64(num) => num as f64,
        Instance::Int128(num) => num as f64,
        Instance::UByte(num) => num as f64,
        Instance::UInt16(num) => num as f64,
        Instance::UInt32(num) => num as f64,
        Instance::UInt64(num) => num as f64,
        Instance::UInt128(num) => num as f64,
        Instance::Float32(num) => num as f64,
        Instance::Float64(num) => num,
        _ => panic!()
    };
    Float64(value)
}

pub(crate) mod byte {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Byte"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod ubyte {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.UByte"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod int16 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Int16"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod uint16 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.UInt16"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod int32 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Int32"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod uint32 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.UInt32"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod int64 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Int64"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod uint64 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.UInt64"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod int128 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Int128"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod uint128 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.UInt128"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod float32 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Float32"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}

pub(crate) mod float64 {
    use crate::_type::{TypeBuilder, TypeRegistry, TypeRef};
    use crate::string_pool::StringPool;
    
    use std::rc::Rc;
    use std::cell::RefCell;

    pub(crate) fn create(string_pool: &mut Rc<RefCell<StringPool>>, type_registry: &mut Rc<RefCell<TypeRegistry>>) {
        let _type = TypeBuilder::new(string_pool.borrow_mut().pool_str("spool.core.number.Float64"))
            .supertype(TypeRef::new(Rc::clone(type_registry), string_pool.borrow_mut().pool_str("spool.core.number.Number")))
            .build();
        type_registry.borrow_mut().register(_type)
    }
}