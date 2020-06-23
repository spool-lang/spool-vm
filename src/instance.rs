use std::rc::Rc;
use crate::instruction::Chunk;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Error, Debug};
use std::fmt;
use crate::vm::{VM, Mut};
use crate::_type::{Type, TypeRef, Property};
use std::collections::HashMap;
use rand::prelude::ThreadRng;
use std::iter::FromIterator;
use rand::Rng;

// Represents instances created at runtime
#[derive(Clone, Debug)]
pub(crate) enum Instance {
    // Represents both instances of the builtin object type & instances
    // of non-builtin subtypes.
    Object(Mut<Type>, InstanceData),
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
    Array(Box<[Instance]>, Mut<Type>),
    //Represents a class object.
    //Class(Box<Class>)
    //Represents a function.
    Func(Function),
    Void
}

impl Instance {
    pub fn get_canonical_name(&self) -> Rc<String> {
        let _ref;
        let name;

        let string = match &self {
            Instance::Object(_type, _) => {
                _ref = _type.borrow();
                name = _ref.canonical_name.clone();
                name.as_str()
            },
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
        }.to_string();

        Rc::new(string)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        return match self {
            Instance::Object(_, _) => write!(f, "{}", self.get_canonical_name()),
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
                let mut array_string = format!("{}[", _type.borrow().get_canonical_name());

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
pub(crate) enum Function {
    Standard(Vec<TypeRef>, Rc<Chunk>),
    Instance(TypeRef, Vec<TypeRef>, Rc<Chunk>),
    Native(u8, fn(&mut VM, Vec<Instance>) -> Instance),
    NativeInstance(u8, fn(&mut VM, Instance, Vec<Instance>) -> Instance),
    NativeConstructor(u8, fn(&mut VM, &Instance, Vec<Instance>))
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        return match self {
            Function::Standard(_, _) => write!(f, "{:?}", "function"),
            Function::Instance(_,_,_) => write!(f, "{:?}", "function"),
            Function::Native(_, _) => write!(f, "{:?}", "native_function"),
            Function::NativeInstance(_, _) => write!(f, "{:?}", "native_function"),
            Function::NativeConstructor(_, _) => write!(f, "{:?}", "native_function")
        }
    }
}

#[derive(Debug)]
pub struct Field {
    property: Mut<Property>,
    value: Option<Instance>,
    initialized: bool
}

impl Field {
    fn new(property: Mut<Property>) -> Field {
        Field {
            property,
            value: None,
            initialized: false
        }
    }

    // TODO: Better error handling.
    fn set(&mut self, value: Instance, value_type: Mut<Type>) {
        if !self.initialized || self.property.borrow().writable {
            self.value = Some(value);
            self.initialized = true;
            return;
        }
        panic!()
    }

    // TODO: Better error handling.
    fn get(&self) -> Instance {
        return (self.value.as_ref().unwrap()).clone()
    }
}

#[derive(Debug)]
pub struct NativeField {
    value: Option<NativeValue>
}

impl NativeField {
    fn get(&self) -> NativeValue {
        return (self.value.as_ref().unwrap()).clone()
    }

    fn set(&mut self, value: NativeValue) {
        self.value = Some(value)
    }
}

#[derive(Clone, Debug)]
pub struct InstanceData {
    fields: HashMap<Rc<String>, Mut<Field>>,
    native: HashMap<Rc<String>, Mut<NativeField>>
}

impl InstanceData {
    pub(crate) fn new(prop_map: &HashMap<Rc<String>, Mut<Property>>) -> InstanceData {

        let map: HashMap<Rc<String>, Mut<Field>> = prop_map.iter().map(|(name, prop)| {
            (Rc::clone(name), Rc::new(RefCell::new(Field::new(Rc::clone(prop)))))
        }).collect();

        InstanceData {
            fields: HashMap::from_iter(map),
            native: Default::default()
        }
    }

    pub(crate) fn get(&self, field_name: Rc<String>) -> Instance {
        match self.fields.get(field_name.as_ref()) {
            None => panic!(),
            Some(field) => field.borrow().get(),
        }
    }

    pub(crate) fn set(&self, field_name: Rc<String>, value: Instance, value_type: Mut<Type>) {
        match self.fields.get(field_name.as_ref()) {
            None => panic!(),
            Some(field) => field.borrow_mut().set(value, value_type)
        }
    }

    pub(crate) fn get_native(&self, field_name: Rc<String>) -> NativeValue {
        match self.native.get(field_name.as_ref()) {
            None => panic!(),
            Some(field) => { field.borrow().get() },
        }
    }

    pub(crate) fn set_native(&self, field_name: Rc<String>, value: NativeValue) {
        match self.native.get(field_name.as_ref()) {
            None => panic!(),
            Some(field) => field.borrow_mut().set(value),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum NativeValue {
    ThreadRng(Box<ThreadRng>)
}