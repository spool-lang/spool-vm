use crate::instance::Instance;
use crate::opcode::{Chunk, OpCode};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

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
            OpCode::Get(from_chunk, index) => self.get(index, from_chunk),
            OpCode::Declare(val, index) => self.declare(index),
            OpCode::Set(index) => panic!(),
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

    fn declare(&mut self, index: &u16) {
        let instance = self.pop_stack();
        self.register.set(instance, index)
    }

    fn set(&mut self) {

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
    entries: HashMap<u16, RefCell<RegisterEntry>>
}

impl NewRegister {
    fn new() -> NewRegister {
        NewRegister {
            entries: Default::default()
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

    fn set(&mut self, instance: Instance, index: &u16) {
        match self.entries.get(index) {
            None => {
                let entry = RegisterEntry::new(instance, true);
                self.entries.insert(*index, RefCell::new(entry));
            },
            Some(entry) => {
                entry.borrow_mut().set(instance)
            },
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