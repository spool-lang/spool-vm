use crate::instance::{Instance, Type};
use crate::opcode::{Chunk, OpCode};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use crate::instance::Instance::*;
use crate::string_pool::StringPool;
use std::sync::Arc;

type Mut<T> = Rc<RefCell<T>>;
type MutVec<T> = Vec<Mut<T>>;

pub struct NewVM {
    stack: Vec<Instance>,
    frame_stack: MutVec<NewCallFrame>,
    register: VMRegister
}

impl NewVM {
    pub fn new() -> NewVM {
        NewVM {
            stack: vec![],
            frame_stack: vec![],
            register: VMRegister::new()
        }
    }

    pub fn run(&mut self, chunk: Chunk) {
        let frame = NewCallFrame::new(chunk);
        self.push_call_frame(frame);
        self.execute()
    }

    fn push_call_frame(&mut self, frame: NewCallFrame) {
        &self.frame_stack.push(Rc::new(RefCell::new(frame)));
    }

    fn get_call_frame(&self) -> Mut<NewCallFrame> {
        let index = self.frame_stack.len();
        let option = self.frame_stack.get(index - 1);
        match option {
            None => panic!(),
            Some(frame) => return Rc::clone(frame),
        }
    }

    fn execute(&mut self) {
        loop {
            let chunk = self.get_call_frame().borrow_mut().get_chunk();
            let pc = self.get_call_frame().borrow_mut().pc;
            let option = chunk.get(pc);

            let result = match chunk.get(pc) {
                None => InstructionResult::Return,
                Some(instruction) => self.execute_instruction(instruction),
            };
            match result {
                InstructionResult::Next => {
                    self.get_call_frame().borrow_mut().pc += 1;
                },
                InstructionResult::Return => break,
                InstructionResult::Jump(index) => panic!(),
            }
        }
    }

    fn execute_instruction(&mut self, instruction: &OpCode) -> InstructionResult {
        match instruction {
            OpCode::GetTrue => self.push_stack(Bool(true)),
            OpCode::GetFalse => self.push_stack(Bool(false)),
            OpCode::Get(from_chunk, index) => self.get(index, from_chunk),
            OpCode::Declare(writable, index) => self.declare(writable),
            OpCode::Set(index) => self.set(index),
            OpCode::Add => self.add(),
            OpCode::Subtract => self.subtract(),
            OpCode::Multiply => self.multiply(),
            OpCode::Divide => self.divide(),
            OpCode::Power => self.pow(),
            OpCode::IntNegate => self.num_negate(),
            OpCode::Greater => self.greater(),
            OpCode::Less => self.less(),
            OpCode::Eq => self.eq(),
            OpCode::GreaterOrEq => self.greater_eq(),
            OpCode::LessOrEq => self.less_eq(),
            OpCode::NotEq => self.not_eq(),
            OpCode::LogicNegate => self.logic_negate(),
            OpCode::Print => println!("{}", self.pop_stack()),
            _ => panic!("This instruction is unimplemented!")
        }
        return InstructionResult::Next
    }

    fn get(&mut self, index: &u16, from_chunk: &bool) {
        let chunk = self.get_call_frame().borrow_mut().get_chunk();
        let instance = if *from_chunk { chunk.get_const(*index) } else { self.register.get(index) };
        self.push_stack(instance)
    }

    fn declare(&mut self, writable: &bool) {
        let instance = self.pop_stack();
        self.register.declare(instance, writable)
    }

    fn set(&mut self, index: &u16) {
        let instance = self.pop_stack();
        self.register.set(instance, index)
    }

    fn add(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let result = match (left, right) {
            (Byte(left_num), Byte(right_num)) => Byte(left_num + right_num),
            (UByte(left_num), UByte(right_num)) => UByte(left_num + right_num),
            (Int16(left_num), Int16(right_num)) => Int16(left_num + right_num),
            (UInt16(left_num), UInt16(right_num)) => UInt16(left_num + right_num),
            (Int32(left_num), Int32(right_num)) => Int32(left_num + right_num),
            (UInt32(left_num), UInt32(right_num)) => UInt32(left_num + right_num),
            (Int64(left_num), Int64(right_num)) => Int64(left_num + right_num),
            (UInt64(left_num), UInt64(right_num)) => UInt64(left_num + right_num),
            (Int128(left_num), Int128(right_num)) => Int128(left_num + right_num),
            (UInt128(left_num), UInt128(right_num)) => UInt128(left_num + right_num),
            (Float32(left_num), Float32(right_num)) => Float32(left_num + right_num),
            (Float64(left_num), Float64(right_num)) => Float64(left_num + right_num),
            _ => panic!("The operands cannot be added!")
        };
        self.push_stack(result)
    }

    fn subtract(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let result = match (left, right) {
            (Byte(left_num), Byte(right_num)) => Byte(left_num - right_num),
            (UByte(left_num), UByte(right_num)) => UByte(left_num - right_num),
            (Int16(left_num), Int16(right_num)) => Int16(left_num - right_num),
            (UInt16(left_num), UInt16(right_num)) => UInt16(left_num - right_num),
            (Int32(left_num), Int32(right_num)) => Int32(left_num - right_num),
            (UInt32(left_num), UInt32(right_num)) => UInt32(left_num - right_num),
            (Int64(left_num), Int64(right_num)) => Int64(left_num - right_num),
            (UInt64(left_num), UInt64(right_num)) => UInt64(left_num - right_num),
            (Int128(left_num), Int128(right_num)) => Int128(left_num - right_num),
            (UInt128(left_num), UInt128(right_num)) => UInt128(left_num - right_num),
            (Float32(left_num), Float32(right_num)) => Float32(left_num - right_num),
            (Float64(left_num), Float64(right_num)) => Float64(left_num - right_num),
            _ => panic!("The operands cannot be subtracted!")
        };
        self.push_stack(result)
    }

    fn multiply(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let result = match (left, right) {
            (Byte(left_num), Byte(right_num)) => Byte(left_num * right_num),
            (UByte(left_num), UByte(right_num)) => UByte(left_num * right_num),
            (Int16(left_num), Int16(right_num)) => Int16(left_num * right_num),
            (UInt16(left_num), UInt16(right_num)) => UInt16(left_num * right_num),
            (Int32(left_num), Int32(right_num)) => Int32(left_num * right_num),
            (UInt32(left_num), UInt32(right_num)) => UInt32(left_num * right_num),
            (Int64(left_num), Int64(right_num)) => Int64(left_num * right_num),
            (UInt64(left_num), UInt64(right_num)) => UInt64(left_num * right_num),
            (Int128(left_num), Int128(right_num)) => Int128(left_num * right_num),
            (UInt128(left_num), UInt128(right_num)) => UInt128(left_num * right_num),
            (Float32(left_num), Float32(right_num)) => Float32(left_num * right_num),
            (Float64(left_num), Float64(right_num)) => Float64(left_num * right_num),
            _ => panic!("The operands cannot be multiplied!")
        };
        self.push_stack(result)
    }

    fn divide(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let result = match (left, right) {
            (Byte(left_num), Byte(right_num)) => Byte(left_num / right_num),
            (UByte(left_num), UByte(right_num)) => UByte(left_num / right_num),
            (Int16(left_num), Int16(right_num)) => Int16(left_num / right_num),
            (UInt16(left_num), UInt16(right_num)) => UInt16(left_num / right_num),
            (Int32(left_num), Int32(right_num)) => Int32(left_num / right_num),
            (UInt32(left_num), UInt32(right_num)) => UInt32(left_num / right_num),
            (Int64(left_num), Int64(right_num)) => Int64(left_num / right_num),
            (UInt64(left_num), UInt64(right_num)) => UInt64(left_num / right_num),
            (Int128(left_num), Int128(right_num)) => Int128(left_num / right_num),
            (UInt128(left_num), UInt128(right_num)) => UInt128(left_num / right_num),
            (Float32(left_num), Float32(right_num)) => Float32(left_num / right_num),
            (Float64(left_num), Float64(right_num)) => Float64(left_num / right_num),
            _ => panic!("The operands cannot be divided!")
        };
        self.push_stack(result)
    }

    fn pow(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let result = match (left, right) {
            (Byte(left_num), Byte(right_num)) => Byte(left_num.pow(right_num.try_into().unwrap())),
            (UByte(left_num), UByte(right_num)) => UByte(left_num.pow(right_num.try_into().unwrap())),
            (Int16(left_num), Int16(right_num)) => Int16(left_num.pow(right_num.try_into().unwrap())),
            (UInt16(left_num), UInt16(right_num)) => UInt16(left_num.pow(right_num.try_into().unwrap())),
            (Int32(left_num), Int32(right_num)) => Int32(left_num.pow(right_num.try_into().unwrap())),
            (UInt32(left_num), UInt32(right_num)) => UInt32(left_num.pow(right_num.try_into().unwrap())),
            (Int64(left_num), Int64(right_num)) => Int64(left_num.pow(right_num.try_into().unwrap())),
            (UInt64(left_num), UInt64(right_num)) => UInt64(left_num.pow(right_num.try_into().unwrap())),
            (Int128(left_num), Int128(right_num)) => Int128(left_num.pow(right_num.try_into().unwrap())),
            (UInt128(left_num), UInt128(right_num)) => UInt128(left_num.pow(right_num.try_into().unwrap())),
            (Float32(left_num), Float32(right_num)) => Float32(left_num.powf(right_num.try_into().unwrap())),
            (Float64(left_num), Float64(right_num)) => Float64(left_num.powf(right_num.try_into().unwrap())),
            _ => panic!("The operands cannot be divided!")
        };
        self.push_stack(result)
    }

    fn num_negate(&mut self) {
        let instance = self.pop_stack();
        let result = match instance {
            Byte(num) => Byte(-num),
            Int16(num) => Int16(-num),
            Int32(num) => Int32(-num),
            Int64(num) => Int64(-num),
            Int128(num) => Int128(-num),
            Float32(num) => Float32(-num),
            Float64(num) => Float64(-num),
            _ => panic!("Operand cannot be negated!")
        };
        self.push_stack(result)
    }

    fn greater(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(self.order(&left, &right));
        self.push_stack(value)
    }

    fn less(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(!self.order(&left, &right));
        self.push_stack(value)
    }

    fn eq(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(self.equate(&left, &right));
        self.push_stack(value)
    }

    fn greater_eq(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(self.order(&left, &right) || self.equate(&left, &right));
        self.push_stack(value)
    }

    fn less_eq(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(!self.order(&left, &right) || self.equate(&left, &right));
        self.push_stack(value)
    }

    fn not_eq(&mut self) {
        let left = self.pop_stack();
        let right = self.pop_stack();
        let value = Bool(!self.equate(&left, &right));
        self.push_stack(value)
    }

    fn order(&mut self, left: &Instance, right: &Instance) -> bool {
        match (left, right) {
            (Byte(left_num), Byte(right_num)) => left_num > right_num,
            (UByte(left_num), UByte(right_num)) => left_num > right_num,
            (Int16(left_num), Int16(right_num)) => left_num > right_num,
            (UInt16(left_num), UInt16(right_num)) => left_num > right_num,
            (Int32(left_num), Int32(right_num)) => left_num > right_num,
            (UInt32(left_num), UInt32(right_num)) => left_num > right_num,
            (Int64(left_num), Int64(right_num)) => left_num > right_num,
            (UInt64(left_num), UInt64(right_num)) => left_num > right_num,
            (Int128(left_num), Int128(right_num)) => left_num > right_num,
            (UInt128(left_num), UInt128(right_num)) => left_num > right_num,
            (Float32(left_num), Float32(right_num)) => left_num > right_num,
            (Float64(left_num), Float64(right_num)) => left_num > right_num,
            _ => panic!("The operands cannot be ordered!")
        }
    }

    fn equate(&mut self, left: &Instance, right: &Instance) -> bool {
        match (left, right) {
            (Byte(left_num), Byte(right_num)) => left_num == right_num,
            (UByte(left_num), UByte(right_num)) => left_num == right_num,
            (Int16(left_num), Int16(right_num)) => left_num == right_num,
            (UInt16(left_num), UInt16(right_num)) => left_num == right_num,
            (Int32(left_num), Int32(right_num)) => left_num == right_num,
            (UInt32(left_num), UInt32(right_num)) => left_num == right_num,
            (Int64(left_num), Int64(right_num)) => left_num == right_num,
            (UInt64(left_num), UInt64(right_num)) => left_num == right_num,
            (Int128(left_num), Int128(right_num)) => left_num == right_num,
            (UInt128(left_num), UInt128(right_num)) => left_num == right_num,
            (Float32(left_num), Float32(right_num)) => left_num == right_num,
            (Float64(left_num), Float64(right_num)) => left_num == right_num,
            (Bool(left_bool), Bool(right_bool)) => left_bool == right_bool,
            _ => panic!("The operands cannot be equated!")
        }
    }

    fn logic_negate(&mut self) {
        let instance = self.pop_stack();
        if let Bool(b) = instance {
            self.push_stack(Bool(!b));
            return;
        }
        panic!("Logic negation can only be applied to booleans!")
    }

    fn push_stack(&mut self, instance: Instance) {
        self.stack.push(instance)
    }

    fn pop_stack(&mut self) -> Instance {
        return match self.stack.pop() {
            None => panic!("Attempted to pop an empty stack!"),
            Some(instance) => instance,
        }
    }
}

struct NewCallFrame {
    chunk: Rc<Chunk>,
    pc: usize,
    stack_top: usize,
    register_top: usize
}

impl NewCallFrame {
    fn new(chunk: Chunk) -> NewCallFrame {
        NewCallFrame {
            chunk: Rc::new(chunk),
            pc: 0,
            stack_top: 0,
            register_top: 0
        }
    }

    fn get_chunk(&self) -> Rc<Chunk> {
        return Rc::clone(&self.chunk)
    }
}

struct VMRegister {
    entries: HashMap<u16, RefCell<RegisterEntry>>,
    size: u16
}

impl VMRegister {
    fn new() -> VMRegister {
        VMRegister {
            entries: Default::default(),
            size: 0
        }
    }

    fn get(&self, index: &u16) -> Instance {
        match self.entries.get(index) {
            None => panic!(),
            Some(entry) => {
                entry.borrow_mut().get()
            },
        }
    }

    fn declare(&mut self, instance: Instance, writable: &bool) {
        let index = self.size;
        let entry = RegisterEntry::new(instance, *writable);
        self.entries.insert(index as u16, RefCell::new(entry));
        self.size += 1;
    }

    fn set(&mut self, instance: Instance, index: &u16) {
        match self.entries.get(index) {
            None => panic!("Attempted to set undeclared variable!"),
            Some(entry) => entry.borrow_mut().set(instance),
        }
    }

    fn get_size(&self) -> u16 {
        self.size
    }

    fn truncate(&mut self, new_size: u16) {
        while self.size > new_size {
            let index = self.size - 1;
            self.entries.remove(&index);
            self.size -= 1;
        }
    }
}

struct RegisterEntry {
    instance: Instance,
    writable: bool
}

impl RegisterEntry {
    fn new(instance: Instance, writable: bool) -> RegisterEntry {
        RegisterEntry {
            instance,
            writable
        }
    }

    fn get(&self) -> Instance {
        return (&self.instance).clone()
    }

    fn set(&mut self, new: Instance) {
        if self.writable {
            self.instance = new;
            return;
        }
        panic!()
    }
}

struct TypeRegistry {
    types: HashMap<u16, Rc<Type>>,
    ids: HashMap<Rc<String>, u16>,
    size: u16
}

impl TypeRegistry {
    fn new(string_pool: &mut StringPool) -> TypeRegistry {
        let mut _self = TypeRegistry {
            types: Default::default(),
            ids: Default::default(),
            size: 0
        };
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Object")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Boolean")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Byte")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.UByte")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Int16")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.UInt16")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Int32")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.UInt32")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Int64")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.UInt64")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Int128")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.UInt128")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Float32")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Float64")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Char")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.String")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Array")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Func")));
        _self.register(Type::new(string_pool.pool_str("silicon.lang.Void")));
        _self
    }

    fn register(&mut self, _type: Type) {
        let index = self.size;
        let name = Rc::clone(&_type.canonical_name);

        self.size += 1;

        self.ids.insert(name, index);
        self.types.insert(index, Rc::from(_type));
    }

    fn get(&self, index: u16) -> Rc<Type> {
        match self.types.get(&index) {
            None => panic!("Attempted to retrieve non-existent type!"),
            Some(t) => Rc::clone(t),
        }
    }

    fn get_by_name(&self, name: Rc<String>) -> Rc<Type> {
        match self.ids.get(&name) {
            None => panic!("Type {} does not exist.", name),
            Some(i) => self.get(*i),
        }
    }
}

#[derive(Debug)]
enum InstructionResult {
    Next,
    Return,
    Jump(u16)
}