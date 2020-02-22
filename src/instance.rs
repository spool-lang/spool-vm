use std::rc::Rc;
use crate::instruction::Chunk;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Error, Debug};
use std::fmt;
use crate::vm::{NewVM};
use crate::_type::Type;

// Represents instances created at runtime
#[derive(Clone, Debug)]
pub enum Instance {
    Object,
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
    Array(Rc<RefCell<Vec<Instance>>>, Rc<Type>),
    //Represents a custom class instance.
    //ClassInstance(Box<ClassInstance>),
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
                Instance::Object => "silicon.core.Object",
                Instance::Bool(_) => "silicon.core.Boolean",
                Instance::Byte(_) => "silicon.core.Byte",
                Instance::UByte(_) => "silicon.core.UByte",
                Instance::Int16(_) => "silicon.core.Int16",
                Instance::UInt16(_) => "silicon.core.UInt16",
                Instance::Int32(_) => "silicon.core.Int32",
                Instance::UInt32(_) => "silicon.core.UInt32",
                Instance::Int64(_) => "silicon.core.Int64",
                Instance::UInt64(_) => "silicon.core.UInt64",
                Instance::Int128(_) => "silicon.core.Int128",
                Instance::UInt128(_) => "silicon.core.UInt128",
                Instance::Float32(_) => "silicon.core.Float32",
                Instance::Float64(_) => "silicon.core.Float64",
                Instance::Char(_) => "silicon.core.Char",
                Instance::Str(_) => "silicon.core.String",
                Instance::Array(_, _) => "silicon.core.Array",
                Instance::Func(_) => "silicon.core.Func",
                Instance::Void => "silicon.core.Void",
                _ => ""
            }.to_string()
        )
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        return match self {
            Instance::Object => write!(f, "{}", "object"),
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
                let borrowed = array.borrow_mut();

                if !borrowed.is_empty() {
                    for i in 0..borrowed.len() {
                        match borrowed.get(i) {
                            Some(instance) => {
                                let item_string = format!("{}", instance);
                                array_string.push_str(item_string.as_str());
                                if i != borrowed.len() - 1 {
                                    array_string.push_str(", ")
                                }
                            }
                            None => panic!("Could not format array!")
                        }

                    }
                }

                write!(f, "{}]", array_string)
            },
            Instance::Func(_) => write!(f, "{}", "function"),
            Instance::Void => write!(f, "{}", "void"),
        };
    }
}

#[derive(Clone)]
pub enum Function {
    Standard(Vec<Rc<Type>>, Rc<Chunk>),
    Native(u8, fn(&mut NewVM, Vec<Instance>) -> Instance),
    NativeInstance(u8, fn(&mut NewVM, Instance, Vec<Instance>) -> Instance)
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

#[derive(Debug)]
pub struct Variable {
    pub(crate) is_const: bool,
    pub(crate) stored: Instance,
    pub(crate) _type: Rc<Type>
}

impl Variable {

    pub(crate) fn new(is_const: bool, stored: Instance, _type: Rc<Type>) -> Variable {
        Variable {
            is_const,
            stored,
            _type
        }
    }

    pub(crate) fn set(&mut self, instance: Instance) {
        if self.is_const {
            panic!("Attempted to set constant variable!")
        }

        if true {
            self.stored = instance;
            return;
        }
        panic!("Type mismatch!")
    }
}