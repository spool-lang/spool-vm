use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryInto, TryFrom};
use std::sync::Arc;

use crate::instance::Instance::*;
use crate::string_pool::StringPool;
use crate::instance::{Instance, Function};
use crate::instruction::{Chunk, Instruction};
use crate::vm::InstructionResult::{GoTo, Next, ReturnVoid, ReturnValue};
use crate::_type::{Type, TypeRegistry};

type Mut<T> = Rc<RefCell<T>>;
type MutVec<T> = Vec<Mut<T>>;

pub struct VM {
    stack: Vec<Instance>,
    type_stack: Vec<Rc<Type>>,
    frame_stack: MutVec<NewCallFrame>,
    register: VMRegister,
    string_pool: StringPool,
    type_registry: TypeRegistry
}

impl VM {
    pub fn new() -> VM {
        let mut string_pool = StringPool::new();
        let type_registry = TypeRegistry::new(&mut string_pool);
        VM {
            stack: vec![],
            type_stack: vec![],
            frame_stack: vec![],
            register: VMRegister::new(),
            string_pool,
            type_registry
        }
    }

    pub fn run(&mut self, chunk: Chunk) {
        let frame = NewCallFrame::new(Rc::new(chunk));
        self.push_call_frame(frame);
        self.execute();
    }

    pub fn type_from_name(&self, name: &str) -> Rc<Type> {
        self.type_registry.get(Rc::from(name.to_string()))
    }

    pub fn pool_string(&mut self, string: &str) -> Rc<String> {
        self.string_pool.pool_string(string.to_string())
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

    fn execute(&mut self) -> Instance {
        loop {
            let chunk = self.get_call_frame().borrow_mut().get_chunk();
            let pc = self.get_call_frame().borrow_mut().pc;

            let result = match chunk.get(pc) {
                None => InstructionResult::ReturnVoid,
                Some(instruction) => {
                    self.execute_instruction(instruction)
                },
            };
            match result {
                InstructionResult::Next => {
                    self.get_call_frame().borrow_mut().pc += 1;
                },
                InstructionResult::ReturnVoid => {
                    self.frame_stack.pop();
                    return Void
                },
                InstructionResult::ReturnValue(value) => {
                    self.frame_stack.pop();
                    return value
                }
                InstructionResult::GoTo(index) => {
                    let chunk = self.get_call_frame().borrow_mut().get_chunk();
                    let option = chunk.jump_table.get(&index);
                    match option {
                        None => panic!(),
                        Some(new_pc) => self.get_call_frame().borrow_mut().pc = *new_pc,
                    }
                },
            }
        }
    }

    fn execute_instruction(&mut self, instruction: &Instruction) -> InstructionResult {
        match instruction {
            Instruction::GetTrue => self.push_stack(Bool(true)),
            Instruction::GetFalse => self.push_stack(Bool(false)),
            Instruction::Get(index, from_chunk) => self.get(index, from_chunk),
            Instruction::Declare(writable) => self.declare(writable),
            Instruction::Set(index) => self.set(index),
            Instruction::Add => self.add(),
            Instruction::Subtract => self.subtract(),
            Instruction::Multiply => self.multiply(),
            Instruction::Divide => self.divide(),
            Instruction::Power => self.pow(),
            Instruction::IntNegate => self.num_negate(),
            Instruction::Greater => self.greater(),
            Instruction::Less => self.less(),
            Instruction::Eq => self.eq(),
            Instruction::GreaterOrEq => self.greater_eq(),
            Instruction::LessOrEq => self.less_eq(),
            Instruction::NotEq => self.not_eq(),
            Instruction::Is => self.is(),
            Instruction::LogicNegate => self.logic_negate(),
            Instruction::InitArray(size) => self.init_array(*size),
            Instruction::IndexGet => self.index_get(),
            Instruction::IndexSet => self.index_set(),
            Instruction::Jump(index, conditional) => return self.jump(index, conditional),
            Instruction::ExitScope(to_clear) => self.register.clear_space(*to_clear),
            Instruction::Call => self.call(),
            Instruction::CallInstance(name_index) => self.call_instance(*name_index),
            Instruction::Return(with_value) => return self.return_from(*with_value),
            Instruction::GetType(id) => self.get_type(id),
            Instruction::Print => println!("{}", self.pop_stack()),
            _ => panic!("This instruction is unimplemented!")
        }
        return InstructionResult::Next
    }

    fn get(&mut self, index: &u16, from_chunk: &bool) {
        let chunk = self.get_call_frame().borrow().get_chunk();
        let instance = if *from_chunk {
            chunk.get_const(*index)
        } else {
            let true_index = &self.get_call_frame().borrow().register_size + index;
            self.register.get(&true_index)
        };
        self.push_stack(instance)
    }

    fn declare(&mut self, writable: &bool) {
        let instance = self.pop_stack();
        self.register.declare(instance, writable)
    }

    fn set(&mut self, index: &u16) {
        let instance = self.pop_stack();
        let true_index = &self.get_call_frame().borrow().register_size + index;
        self.register.set(instance, &true_index)
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

    fn is(&mut self) {
        let instance = self.pop_stack();
        let instance_type = self.type_registry.get(instance.get_canonical_name());
        let comparison_type = self.pop_type_stack();

        let result = comparison_type.matches_type(instance_type);
        self.push_stack(Bool(result))
    }

    fn logic_negate(&mut self) {
        let instance = self.pop_stack();
        if let Bool(b) = instance {
            self.push_stack(Bool(!b));
            return;
        }
        panic!("Logic negation can only be applied to booleans!")
    }

    fn init_array(&mut self, size: u16) {
        let object_type = self.type_registry.get(Rc::new("silicon.core.Object".to_string()));
        let mut contents: Vec<Instance> = vec![];
        for _ in 0..size {
            contents.push(self.pop_stack())
        }
        contents.reverse();

        let fixed_array = Array(contents.into_boxed_slice(), object_type);
        self.push_stack(fixed_array)
    }

    fn index_get(&mut self) {
        let indexable = self.pop_stack();
        let index = self.get_usize();
        match indexable {
            Array(arr, _type) => {
                match arr.get(index) {
                    None => panic!(),
                    Some(instance) => self.push_stack(instance.clone())
                }
            }
            Str(string) => {
                let chars: Vec<char> = string.chars().collect();
                match chars.get(index) {
                    None => panic!(),
                    Some(c) => self.push_stack(Char(*c)),
                }
            }
            _ => panic!()
        };
    }

    fn index_set(&mut self) {
        let indexable = self.pop_stack();
        let index = self.get_usize();
        let to_set = self.pop_stack();
        match indexable {
            Array(mut arr, _type) => {
                arr[index] = to_set;
            }
            _ => panic!()
        };
    }

    fn jump(&mut self, index: &u16, conditional: &bool) -> InstructionResult {
        if *conditional {
            let instance = self.pop_stack();
            if let Bool(value) = instance {
                return if value { GoTo(*index) } else { Next }
            }
            panic!("Unexpected type!")
        }
        return GoTo(*index)
    }

    fn call(&mut self) {
        let func = self.pop_stack();
        if let Func(function) = func {
            match function {
                Function::Standard(param_types, chunk) => {
                    let mut args: Vec<Instance> = vec![];
                    for param_type in param_types{
                        let instance = self.pop_stack();
                        let instance_type = self.type_registry.get(instance.get_canonical_name());
                        if !param_type.matches_type(instance_type) {panic!()}

                        args.push(instance)
                    }
                    let stack_size = self.stack.len();
                    let type_stack_size = self.type_stack.len();
                    let register_size = self.register.size;
                    for arg in args {
                        self.register.declare(arg, &false)
                    }
                    let frame = NewCallFrame::new_inner(chunk.clone(), stack_size, type_stack_size, register_size);
                    self.push_call_frame(frame);
                    let returned=  self.execute();

                    self.stack.truncate(stack_size);
                    self.type_stack.truncate(type_stack_size);
                    let new_register_size = self.register.size;
                    self.register.clear_space(new_register_size - register_size);

                    if let Void = returned {} else { self.push_stack(returned) };
                },
                Function::Native(arity, function) => {
                    let mut args: Vec<Instance> = vec![];
                    for x in 0..arity {
                        args.push(self.pop_stack())
                    }
                    let returned= function(self, args);
                    if let Void = returned {
                    } else { self.push_stack(returned) };
                },
                _ => panic!()
            }
        }
        else {
            panic!()
        };
    }

    fn call_instance(&mut self, name_index: u16) {
        let instance = self.pop_stack();
        let instance_type = self.type_registry.get(instance.get_canonical_name());
        let name = self.get_call_frame().borrow().chunk.get_name(name_index);

       match instance_type.get_instance_func(name) {
           Function::NativeInstance(arity, function) => {
               let mut args: Vec<Instance> = vec![];
               for x in 0..arity {
                   args.push(self.pop_stack())
               }
               let returned = function(self, instance, args);
               if let Void = returned {
               } else { self.push_stack(returned) };
           },
           _ => panic!()
       };
    }

    fn return_from(&mut self, with_value: bool) -> InstructionResult {
        if with_value {
            let value = self.pop_stack();
            ReturnValue(value)
        }
        else {
            ReturnVoid
        }
    }

    fn get_type(&mut self, id: &u16) {
        let type_name = match self.get_call_frame().borrow().chunk.name_table.get(id) {
            None => panic!(),
            Some(name) => Rc::clone(name),
        };

        let _type = self.type_registry.get(type_name);
        self.push_type_stack(_type)
    }

    fn get_usize(&mut self) -> usize {
        match self.pop_stack() {
            Byte(val) => usize::try_from(val).unwrap(),
            Int16(val) => usize::try_from(val).unwrap(),
            Int32(val) => usize::try_from(val).unwrap(),
            Int64(val) => usize::try_from(val).unwrap(),
            Int128(val) => usize::try_from(val).unwrap(),
            UByte(val) => val as usize,
            UInt16(val) => val as usize,
            UInt32(val) => val as usize,
            UInt64(val) => val as usize,
            UInt128(val) => val as usize,
            _ => panic!()
        }

    }

    fn push_stack(&mut self, instance: Instance) {
        self.stack.push(instance);
    }

    fn pop_stack(&mut self) -> Instance {
        let size = self.get_call_frame().borrow().stack_size;
        let true_size = self.stack.len();
        if true_size <= size { panic!("Attempted to pop an empty stack!") }

        return match self.stack.pop() {
            None => panic!("Attempted to pop an empty stack!"),
            Some(instance) => instance,
        }
    }

    fn push_type_stack(&mut self, _type: Rc<Type>) {
        self.type_stack.push(_type)
    }

    fn pop_type_stack(&mut self) -> Rc<Type> {
        let size = self.get_call_frame().borrow().type_stack_size;
        let true_size = self.type_stack.len();
        if true_size <= size { panic!("Attempted to pop an empty type stack!") }

        return match self.type_stack.pop() {
            None => panic!("Attempted to pop an empty type stack!"),
            Some(_type) => _type,
        }
    }
}

struct NewCallFrame {
    chunk: Rc<Chunk>,
    pc: usize,
    stack_size: usize,
    type_stack_size: usize,
    register_size: u16
}

impl NewCallFrame {
    fn new(chunk: Rc<Chunk>) -> NewCallFrame {
        NewCallFrame {
            chunk,
            pc: 0,
            stack_size: 0,
            type_stack_size: 0,
            register_size: 0
        }
    }

    fn new_inner(chunk: Rc<Chunk>, stack_size: usize, type_stack_size: usize, register_size: u16) -> NewCallFrame {
        NewCallFrame {
            chunk,
            pc: 0,
            stack_size,
            type_stack_size,
            register_size
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

    fn clear_space(&mut self, to_clear: u16) {
        if self.size == 0 { return; }
        for x  in 0..to_clear {
            let index = self.size - 1;
            self.entries.remove(&index);
            self.size -= 1;
            if self.size == 0 { break; }
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

#[derive(Debug)]
enum InstructionResult {
    Next,
    ReturnVoid,
    ReturnValue(Instance),
    GoTo(u16)
}