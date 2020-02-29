use std::rc::Rc;
use crate::instruction::Chunk;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Error, Debug};
use std::fmt;
use crate::vm::{VM};
use crate::_type::Type;
use std::collections::HashMap;

// Represents instances created at runtime
#[derive(Clone, Debug)]
pub enum Instance {
    // Represents both instances of the builtin object type & instances
    // of non-builtin subtypes.
    Object(Rc<Type>, Rc<RefCell<HashMap<Rc<String>, Instance>>>),
    Bool(bool),
    Byte(i8),
    UByte(u8),
    Int16(i16),
    UInt16(u16),
    Int32(i32),
    UInt32(u32),
    Int64(i64),
    UInt64(u64),
    Int128(i128),
    UInt128(u128),
    //Fixed-point precision.
    //Decimal16(),
    //UDecimal16(),
    //Decimal32(),
    //UDecimal32(),
    //Decimal64(),
    //UDecimal64(),
    //Decimal128(),
    //UDecimal128(),
    Float32(f32),
    Float64(f64),
    //These are commented out for now but I would like to bring in the 'num' crate at some point
    //to introduce these types or make my own.
    //BigInt(),
    //UBigInt(),
    //BigFloat(),
    //BigDecimal(),
    //Complex(),
    Char(char),
    Str(Rc<String>),
    Array(Box<[Instance]>, Rc<Type>),
    //Represents a class object.
    //Class(Box<Class>)
    //Represents a function.
    Func(Function),
    Void
}

impl Instance {
    pub fn get_canonical_name(&self) -> Rc<String> {
        Rc::new(
            match self {
                Instance::Object(_type, _) => _type.canonical_name.as_str(),
                Instance::Bool(_) => "spool.core.Boolean",
                Instance::Byte(_) => "spool.core.number.Byte",
                Instance::UByte(_) => "spool.core.number.UByte",
                Instance::Int16(_) => "spool.core.number.Int16",
                Instance::UInt16(_) => "spool.core.number.UInt16",
                Instance::Int32(_) => "spool.core.number.Int32",
                Instance::UInt32(_) => "spool.core.number.UInt32",
                Instance::Int64(_) => "spool.core.number.Int64",
                Instance::UInt64(_) => "spool.core.number.UInt64",
                Instance::Int128(_) => "spool.core.number.Int128",
                Instance::UInt128(_) => "spool.core.number.UInt128",
                Instance::Float32(_) => "spool.core.number.Float32",
                Instance::Float64(_) => "spool.core.number.Float64",
                Instance::Char(_) => "spool.core.Char",
                Instance::Str(_) => "spool.core.String",
                Instance::Array(_, _) => "spool.core.Array",
                Instance::Func(_) => "spool.core.Func",
                Instance::Void => "spool.core.Void",
                _ => ""
            }.to_string()
        )
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        return match self {
            Instance::Object(_, _) => write!(f, "{}", "object"),
            Instance::Bool(boolean) => write!(f, "{}", boolean),
            Instance::Byte(byte) => write!(f, "{}b", byte),
            Instance::UByte(ubyte) => write!(f, "{}ub", ubyte),
            Instance::Int16(int16) => write!(f, "{}i16", int16),
            Instance::UInt16(uint16) => write!(f, "{}u16", uint16),
            Instance::Int32(int32) => write!(f, "{}i32", int32),
            Instance::UInt32(uint32) => write!(f, "{}ui32", uint32),
            Instance::Int64(int64) => write!(f, "{}i64", int64),
            Instance::UInt64(uint64) => write!(f, "{}u64", uint64),
            Instance::Int128(int128) => write!(f, "{}i128", int128),
            Instance::UInt128(uint128) => write!(f, "{}u128", uint128),
            Instance::Float32(float32) => write!(f, "{}f32", float32),
            Instance::Float64(float64) => write!(f, "{}f64", float64),
            Instance::Char(character) => write!(f, "'{}'", character),
            Instance::Str(string) => write!(f, "\"{}\"", string),
            Instance::Array(array, _type) => {
                let mut array_string = format!("{}[", _type.get_canonical_name());

                for i in 0..array.len() {
                    let instance = array.get(i).unwrap();
                    let instance_string = format!("{}", instance);
                    array_string.push_str(instance_string.as_str());
                    if i != array.len() - 1 {
                        array_string.push_str(", ")
                    }
                }

                write!(f, "{}]", array_string)
            },
            Instance::Func(_) => write!(f, "{}", "function"),
            Instance::Void => write!(f, "{}", "void")
        };
    }
}

#[derive(Clone)]
pub enum Function {
    Standard(Vec<Rc<Type>>, Rc<Chunk>),
    Native(u8, fn(&mut VM, Vec<Instance>) -> Instance),
    NativeInstance(u8, fn(&mut VM, Instance, Vec<Instance>) -> Instance)
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        return match self {
            Function::Standard(_, _) => write!(f, "{:?}", "function"),
            Function::Native(_, _) => write!(f, "{:?}", "native_function"),
            Function::NativeInstance(_, _) => write!(f, "{:?}", "native_function")
        }
    }
}