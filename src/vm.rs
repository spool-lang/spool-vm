use crate::instance::Instance;
use crate::opcode::{Chunk, OpCode};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::instance::Instance::*;

type Mut<T> = Rc<RefCell<T>>;
type MutVec<T> = Vec<Mut<T>>;

pub struct NewVM {
    stack: Vec<Instance>,
    frame_stack: MutVec<NewCallFrame>,
    register: NewRegister
}

impl NewVM {
    pub fn new() -> NewVM {
        NewVM {
            stack: vec![],
            frame_stack: vec![],
            register: NewRegister::new()
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
            println!("{:?}", result);
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

struct NewRegister {
    entries: HashMap<u16, RefCell<RegisterEntry>>,
    size: usize
}

impl NewRegister {
    fn new() -> NewRegister {
        NewRegister {
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
    Return,
    Jump(u16)
}