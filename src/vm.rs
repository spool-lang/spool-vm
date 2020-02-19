use crate::instance::Instance;
use crate::opcode::{Chunk, OpCode};
use std::rc::Rc;
use std::cell::RefCell;

type Mut<T> = Rc<RefCell<T>>;
type MutVec<T> = Vec<Mut<T>>;

pub struct NewVM {
    stack: Vec<Instance>,
    frame_stack: MutVec<NewCallFrame>
}

impl NewVM {
    pub fn new() -> NewVM {
        NewVM {
            stack: vec![],
            frame_stack: vec![]
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
            OpCode::Get(from_chunk, index) => self.get(index),
            OpCode::Declare(val, constant) => panic!(),
            OpCode::Set(index) => panic!(),
            OpCode::Print => println!("{}", self.pop_stack()),
            _ => panic!("This instruction is unimplemented!")
        }
        return InstructionResult::Next
    }

    fn get(&mut self, index: &u16) {
        let chunk = self.get_call_frame().borrow_mut().get_chunk();
        let instance = chunk.get_const(*index);
        self.push_stack(instance)
    }

    fn declare(&mut self) {

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

#[derive(Debug)]
enum InstructionResult {
    Next,
    Return,
    Jump(u16)
}